use std::env;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::io::{self};
use std::path::PathBuf;

use std::process::{Command, Stdio};
use std::thread;

pub(crate) fn compile(path: PathBuf, _output: Option<PathBuf>, params: Vec<String>) {
    let model_txt = fs::read_to_string(&path).expect("Failed to read model file");
    let _template_path = match create_template() {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Failed to create template: {}", e);
            return;
        }
    };
    let template_path = match inject_model(model_txt, params) {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Failed to inject model: {}", e);
            return;
        }
    };
    let dynlib_path = match build_template(template_path.clone()) {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Failed to build template: {}", e);
            return;
        }
    };
    let output_path = _output.unwrap_or_else(|| {
        let default_name = format!("model_{}_{}.pkm", env::consts::OS, env::consts::ARCH);
        path.with_file_name(default_name)
    });

    fs::copy(&dynlib_path, &output_path).expect("Failed to copy dynamic library to output path");
}

pub(crate) fn dummy_compile() -> Result<String, io::Error> {
    let template_path = create_template()?;
    build_template(template_path.clone())?;
    Ok(template_path.to_string_lossy().to_string())
}

fn stream_output<R: Read + Send + 'static>(
    reader: R,
    mut writer: impl Write + Send + 'static,
) -> thread::JoinHandle<Result<(), io::Error>> {
    thread::spawn(move || {
        let mut buffer = [0; 4096];
        let mut reader = io::BufReader::new(reader);

        loop {
            let n = reader.read(&mut buffer)?;
            if n == 0 {
                break;
            }
            writer.write_all(&buffer[..n])?;
            writer.flush()?;
        }
        Ok(())
    })
}

fn build_template(template_path: PathBuf) -> Result<PathBuf, io::Error> {
    let mut command = Command::new("cargo");
    command
        .arg("build")
        .arg("--release")
        .current_dir(&template_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = command.spawn()?;

    let stdout = child.stdout.take().expect("Failed to capture stdout");
    let stderr = child.stderr.take().expect("Failed to capture stderr");

    let stdout_handle = stream_output(stdout, io::stdout());
    let stderr_handle = stream_output(stderr, io::stderr());

    let status = child.wait()?;
    stdout_handle
        .join()
        .expect("Failed to join stdout thread")?;
    stderr_handle
        .join()
        .expect("Failed to join stderr thread")?;

    if !status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Failed to build the template",
        ));
    }

    let dynlib_name = if cfg!(target_os = "windows") {
        "model_lib.dll"
    } else if cfg!(target_os = "macos") {
        "libmodel_lib.dylib"
    } else {
        "libmodel_lib.so"
    };

    Ok(template_path
        .join("target")
        .join("release")
        .join(dynlib_name))
}

pub(crate) fn template_path() -> String {
    env::temp_dir()
        .join("exa_tmp")
        .join("template")
        .to_string_lossy()
        .to_string()
}

pub(crate) fn clear_build() {
    let temp_dir = env::temp_dir().join("exa_tmp");
    if temp_dir.exists() {
        fs::remove_dir_all(temp_dir).expect("Failed to remove temporary directory");
    }
}

fn create_template() -> Result<PathBuf, io::Error> {
    let temp_dir = env::temp_dir().join("exa_tmp");
    if !temp_dir.exists() {
        fs::create_dir_all(&temp_dir)?;
    }
    let template_dir = temp_dir.join("template");
    let cargo_toml_path = template_dir.join("Cargo.toml");

    let cargo_toml_content = r#"
        [package]
        name = "model_lib"
        version = "0.1.0"
        edition = "2021"

        [lib]
        crate-type = ["cdylib"]

        [dependencies]
        pmcore = { git = "https://github.com/LAPKB/PMcore.git", rev="b865eb863e9b44ddb2df41e64ce83d65838c72ea" }
        "#;

    if !template_dir.exists() {
        let output = Command::new("cargo")
            .arg("new")
            .arg("template")
            .arg("--lib")
            .current_dir(&temp_dir)
            .output()
            .expect("Failed to create cargo project");

        io::stderr().write_all(&output.stderr)?;
        io::stdout().write_all(&output.stdout)?;

        fs::write(cargo_toml_path, cargo_toml_content)?;
    } else if !cargo_toml_path.exists() {
        fs::write(cargo_toml_path, cargo_toml_content)?;
    };
    Ok(template_dir)
}

fn inject_model(model_txt: String, params: Vec<String>) -> Result<PathBuf, io::Error> {
    let template_dir = env::temp_dir().join("exa_tmp").join("template");
    let lib_rs_path = template_dir.join("src").join("lib.rs");
    let lib_rs_content = format!(
        r#"
        #![allow(dead_code)]
        #![allow(unused_variables)]
        use std::ffi::c_void;
        use pmcore::prelude::*;
    
        pub fn eqn() -> ODE {{
            {}
        }}
    
        #[no_mangle]
        pub extern "C" fn create_eqn_ptr() -> *mut c_void {{
            let eqn = Box::new(eqn());
            Box::into_raw(eqn) as *mut c_void
        }}
    
       #[no_mangle]
        pub extern "C" fn metadata_ptr() -> *mut c_void{{
        let meta = Box::new(equation::Meta::new(vec![{}]));
        Box::into_raw(meta) as *mut c_void
        }}
        "#,
        model_txt,
        params
            .iter()
            .map(|p| format!("\"{}\"", p))
            .collect::<Vec<String>>()
            .join(", ")
    );
    fs::write(lib_rs_path, lib_rs_content)?;
    Command::new("cargo")
        .arg("fmt")
        .current_dir(&template_dir)
        .output()
        .expect("Failed to format cargo project");
    Ok(template_dir)
}

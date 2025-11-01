#[cfg(test)]
mod panic_hook_tests {
    use std::panic;
    use std::sync::Once;

    static INIT: Once = Once::new();

    /// Initialize test panic hook (simplified version for testing)
    fn init_test_panic_hook() {
        INIT.call_once(|| {
            panic::set_hook(Box::new(|panic_info| {
                let location = panic_info
                    .location()
                    .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()))
                    .unwrap_or_else(|| "unknown location".to_string());

                let message = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
                    s.clone()
                } else {
                    "unknown panic payload".to_string()
                };

                println!("PANIC CAUGHT: {} at {}", message, location);
            }));
        });
    }

    #[test]
    #[should_panic(expected = "test panic")]
    fn test_panic_is_caught() {
        init_test_panic_hook();
        panic!("test panic");
    }

    #[test]
    fn test_panic_hook_initialization() {
        // This test verifies that the panic hook can be initialized
        init_test_panic_hook();
        // If we get here, initialization succeeded
        assert!(true);
    }
}

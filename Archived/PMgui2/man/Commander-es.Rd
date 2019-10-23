\name{Commander-es}
\alias{Commander-es}
\encoding{utf-8}

\title{R Commander}
\description{
  Inicia la GUI (Interfaz \enc{Gráfica}{Grafica} de Usuario) de R Commander
}
\usage{
Commander()
}

\details{

 \bold{Empezando}

La interfaz por defecto de R Commander consiste en (de arriba a abajo) una barra de \enc{menús}{menus}, una barra de herramientas, una ventana de instrucciones, una ventana de salida y una ventana de mensajes. 

Las instrucciones para leer, escribir, transformar y analizar datos se ejecutan usando la barra de \enc{menú}{menu} de la parte superior de la ventana de \emph{R Commander}. La mayor parte de los items de este \enc{menú}{menu} le \enc{guiarán}{guiaran} mediante ventanas de \enc{diálogo}{dialogo}, preguntando \enc{más}{mas} \enc{allá}{alla} de la \enc{especificación}{especificacion}. Es aconsejable explorar el \enc{menú}{menu} para ver las opciones disponibles. 

Bajo la barra de \enc{menú}{menu} se encuentra la barra de herramientas con (de izquierda a derecha) un campo de \enc{información}{informacion} que muestra el nombre del conjunto de datos activos, botones para editar y mostrar el conjunto de datos activos y un campo de \enc{información}{informacion} mostrando el modelo \enc{estadístico}{estadistico} activo. Bajo la ventana de instrucciones hay un \enc{botón}{boton} \code{Ejecutar} para realizar las \enc{órdenes}{ordenes} indicadas en la ventana de instrucciones. Los campos de \enc{información}{informacion} para los datos y el modelo activo son botones que pueden ser usados para seleccionar \enc{éstos}{estos} entre, respectivamente, conjuntos de datos o modelos disponibles en memoria. 

La mayor parte de las \enc{órdenes}{ordenes} requiere un conjunto de datos activos. Cuando se ejecuta R Commander no hay conjunto de datos activos, como \enc{está}{esta} indicado en el campo de \enc{información}{informacion} del conjunto de datos activos. Un conjunto de datos llega a ser un conjunto de datos activos cuando \enc{éste}{este} es \enc{leído}{leido} en la memoria desde un paquete R o importado desde un archivo de texto, conjunto de datos SPSS, conjunto de datos Minitab, conjunto de datos STATA, Excel, Access o dBase. \enc{Además}{Ademas} el conjunto de datos activos puede ser seleccionado desde conjuntos de datos R residentes en memoria. Los datos pueden ser elegidos de entre todos los conjuntos para cada \enc{sesión}{sesion}. 

Por defecto, las \enc{órdenes}{ordenes} son registradas en la ventana de instrucciones (la ventana de texto \enc{vacía}{vacia} inmediatamente \enc{después}{despues} de la barra de herramientas); las \enc{órdenes}{ordenes} y las salidas aparecen en la ventana de resultados (la ventana de texto \enc{vacía}{vacia} \enc{después}{despues} de la ventana de instrucciones) y el conjunto de datos activos es adjuntado a la ruta de \enc{búsqueda}{busqueda}. Para alterar \enc{éstos}{estos} y otros \enc{parámetros}{parametros} por defecto, puede consultar la \enc{información}{informacion} pertinente en \enc{configuración}{configuracion}. 

Algunos \enc{diálogos}{dialogos} de \code{Rcmdr} (\enc{éstos}{estos} en \code{Estadisticos -> Ajuste de modelos}) generan el modelo lineal, modelo lineal generalizado y otros modelos. Cuando un modelo es ajustado, se convierte en el modelo activo, indicado en el campo de \enc{información}{informacion} de la barra de herramientas de R Commander. Los items del \enc{menú}{menu} \code{Modelos} se aplican al modelo activo. Inicialmente, no hay modelo activo. Si hay varios modelos en memoria, puede elegir el modelo activo de entre ellos. 

Si el registro de instrucciones \enc{está}{esta} activo, las \enc{órdenes}{ordenes} de R generadas desde los \enc{menús}{menus} y los cuadros de \enc{diálogos}{dialogos}, son introducidas en la ventana de instrucciones de R Comander. Se pueden editar estas \enc{órdenes}{ordenes} de manera normal y se pueden escribir otras nuevas en la ventana de instrucciones. Las \enc{órdenes}{ordenes} individuales pueden ser continuadas en \enc{más}{mas} de una \enc{línea}{linea}, 
pero cada \enc{línea}{linea} \enc{después}{despues} de la primera debe ser identada con uno o \enc{más}{mas} espacios o tabuladores. El contenido de la ventana de instrucciones puede ser almacenado durante o al final de la \enc{sesión}{sesion} y un conjunto de instrucciones guardado puede ser cargado en la ventana de instrucciones. El contenido de la ventana de resultados puede ser editado o guardado en un archivo de texto. 

Para volver a ejecutar una orden o un conjunto de ellas, se seleccionan las \enc{líneas}{lineas} que se desean ejecutar usando el \enc{ratón}{raton} y se presiona el \enc{botón}{boton} \code{Ejecutar}, situado a la derecha de la barra de herramientas (o \code{Control-R}, para ejecutarlos). Si no hay texto seleccionado el \enc{botón}{boton} \code{Ejecutar} (o \code{Control-R}) \enc{envía}{envia} el contenido de la \enc{línea}{linea} que contiene el cursor de \enc{insercción}{inserccion}. Observar que se \enc{generará}{generara} un error si la orden o las \enc{órdenes}{ordenes} enviadas son incompletas. 

Presionando \code{Control-F} se abre un cuadro de \enc{diálogo}{dialogo} de \enc{búsqueda}{busqueda} de texto (\enc{también}{tambien} es accesible \enc{vía}{via} \code{Editar -> Buscar}) para buscar el texto en la ventana de instrucciones o la ventana de resultados. Las \enc{búsqueda}{busqueda}s son realizadas en la ventana de instrucciones a menos que primero pulse en la ventana de resultados para activarla. 

Presionando \code{Control-S} se \enc{guardará}{guardara} el conjunto de instrucciones o la ventana de resultados. 

Presionando \code{Control-A} se selecciona todo el texto del conjunto de instrucciones o de la ventana de resultados. 

Pulsando el \enc{botón}{boton} derecho del \enc{ratón}{raton} (el tercer \enc{botón}{boton} en un \enc{ratón}{raton} de tres botones) en el conjunto de instrucciones o en la ventana de resultados se abre el \enc{menú}{menu} contextual con los items del \enc{menú}{menu} \code{Editar}, \enc{más}{mas} un item \code{Ejecutar} (en la ventana de instrucciones). 

Cuando ejecute \enc{órdenes}{ordenes} en la ventana de R Commander, debe asegurarse que la sentencia sea \enc{lógica}{logica}. Por ejemplo, no tiene sentido ajustar un modelo \enc{estadístico}{estadistico} de un conjunto de datos que no ha sido \enc{leído}{leido} en memoria. 

Presionando una letra (ej. "a") en un cuadro con una lista se \enc{recorrerá}{recorrera} \enc{ésta}{esta} hasta la siguiente entrada que comience con esa letra desde el principio del cuadro. 

Salir de R Commander se realiza mediante \code{Fichero -> Salir} o cerrando la ventana de R Commander. 

\bold{\enc{Personalización}{Personalizacion} y \enc{configuración}{configuracion}} 

Los archivos de \enc{configuración}{configuracion} \enc{están}{estan} en el subdirectorio \code{etc} de cada paquete o en la \enc{localización}{localizacion} dada por \code{etc} y en las opciones de \code{etcMenus} (mirar abajo). 

Los \enc{menús}{menus} de \code{Rcmdr} pueden ser personalizados editando el archivo \code{Rcmdr-menus.txt}. 

Algunas funciones (ej. \code{histograma}) que normalmente no crean salida visible cuando se ejecutan desde la consola \enc{sí}{si} lo \enc{harán}{haran} - a menos que se evite - cuando se ejecuten desde la ventana de instrucciones de R Commander. Tal salida puede ser suprimida listando los nombres de estas funciones en el archivo \code{log-exceptions.txt}. 

Puede \enc{añadir}{anadir} \enc{código}{codigo} R al paquete, ej., para crear \enc{diálogo}{dialogo}s adicionales, colocando archivos con \enc{extensión}{extension} \code{.R} en el directorio \code{etc}, \enc{además}{ademas} puede editar \code{Rcmdr-menus.txt} para proporcionar \enc{menús}{menus} adicionales, sub\enc{menús}{menus} o items. Una \enc{demostración}{demostracion} de esto se proporciona mediante el archivo \code{BoxCox.demo}. Para activar la demo, renombre el archivo a \code{BoxCox.R} y descomente la correspondiente \enc{línea}{linea} del \enc{menú}{menu} en \code{Rcmdr-menus.txt}. De forma alternativa, puede editar el \enc{código}{codigo} del paquete y recompilarlo. 

Algunas funciones son proporcionadas para ayudar a escribir \enc{diálogo}{dialogo}s y la \enc{información}{informacion} del estado de \code{Rcmdr} en un emplazamiento separado. Mirar \code{help("Rcmdr.Utilities")} y el manual suministrado en el directorio \code{doc} del paquete de \code{Rcmdr} para mayor \enc{información}{informacion}. 

\enc{Además}{Ademas}, varias \enc{características}{caracteristicas} son controladas mediante opciones, en tiempos de \enc{ejecución}{ejecucion}, establecidas por la orden \code{options("Rcmdr")}. Estas opciones deben ser establecidas antes de cargar el paquete. Si las opciones no \enc{están}{estan} establecidas, que es la \enc{situación}{situacion} normal, \enc{serán}{seran} usados los \enc{parámetros}{parametros} por defecto. Las opciones se especifican como una lista de pares \code{name$=$values}. Puede no establecer, establecer una, varias, o todas las opciones. Las opciones disponibles son las dadas a \enc{continuación}{continuacion}: 

\describe{
\item{\code{attach.data.set}}{Si es \code{TRUE} (por defecto
  \code{FALSE}), el conjunto de datos activo es fijado como la ruta de
  \enc{búsqueda}{busqueda}.}

\item{\code{check.packages}}{Si es \code{TRUE} (por defecto), al
  arranque, la presencia de todos los paquetes recomendados de Rcmdr
  \enc{serán}{seran} comprobados y si alguno no \enc{está}{esta} instalado, Rcmdr \enc{preguntará}{preguntara} si deben instalarse.} 

\item{\code{command.text.color}}{El color de las \enc{órdenes}{ordenes} en la ventana de resultados es, por defecto, \code{"red"}.} 

\item{\code{console.output}}{Si es \code{TRUE} la salida \enc{será}{sera} dirigida a
  la consola de R y la ventana de salida de R Commander no se
  \enc{mostrará}{mostrara}. Por defecto es \code{FALSE}.} 

\item{\code{contrasts}}{Ofrece la misma \enc{función}{funcion} que la \enc{opción}{opcion} general
  \code{contrasts}; por defecto es \code{c("contr.Treatment",
    "contr.poly")}. Cuando se sale de Commander la \enc{opción}{opcion}
  \code{contrasts} vuelve a su valor preexistente. Observe que
  \code{contr.Treatment} es del paquete \code{car}.} 

\item{\code{crisp.dialogs}}{Si es \code{TRUE}, los \enc{diálogos}{dialogos} deben
  aparecer en la pantalla dibujada completamente, \enc{más}{mas} que acumular
  dispositivo a dispositivo. Esta \enc{opción}{opcion} \enc{debería}{deberia} afectar \enc{sólo}{solo} a
  versiones Windows de R, pero debe en cualquier caso ser
  inofensivo. Por defecto es \code{TRUE} bajo versiones Windows de R
  2.1.1 y superiores y \code{FALSE} si no. Si \enc{está}{esta} trabajando en
  Windows y encuentra que se incrementan los problemas de estabilidad,
  pruebe establecer esta \enc{opción}{opcion} a \code{FALSE}.}

\item{\code{default.font}}{La fuente por defecto, como la \enc{especificación}{especificacion}
  de la fuente de X11, dada en cadena de caracteres. Si \enc{está}{esta}
  especificado, este valor toma precedencia sobre el \enc{tamaño}{tamano} de la fuente
  por defecto (abajo). Esta \enc{opción}{opcion} es \enc{sólo}{solo} para sistemas no-Windows.} 

\item{\code{default.font.size}}{\enc{Tamaño}{Tamano}, en puntos, por defecto de la
  fuente. Por defecto es 10 para sistemas Windows y 12 para otro
  sistemas, salvo \enc{especificación}{especificacion} de lo contrario (mirar el item
  anterior). La fuente por defecto es
  \code{"*helvetica-medium-r-normal-*-xx*"}, donde \code{xx} es por
  defecto el \enc{tamaño}{tamano} de la fuente. Esta \enc{opción}{opcion} es \enc{sólo}{solo} para sistemas no-Windows.} 

\item{\code{double.click}}{Establecer a \code{TRUE} si quiere que un doble click con el \enc{botón}{boton} izquierdo del \enc{ratón}{raton} sirva para pulsar el \enc{botón}{boton} por defecto en todos los \enc{diálogos}{dialogos}. Por defecto es \code{FALSE}.}

\item{\code{error.text.color}}{Color de los mensajes de error; por
  defecto es \code{"red"}.} 

\item{\code{etc}}{Establece la ruta del directorio que contiene los archivos de \enc{configuración}{configuracion} de Rcmdr; por defecto el subdirectorio \code{etc} del paquete Rcmdr instalado.} 

\item{\code{grab.focus}}{Establecer a \code{TRUE} para "mantener" el enfoque en la ventana actual de Tk, esto es, para prevenir que el enfoque sea cambiado a otra ventana Tk. En algunos sistemas, mantener el enfoque de esta forma, puede causar problemas. Por defecto es TRUE. Si experimenta problemas de enfoque, intente establecer esta \enc{opción}{opcion} a \code{FALSE}.} 

\item{\code{load.at.startup}}{Vector de caracteres de nombres de los paquetes que deben ser cargados cuando el paquete \code{Rcmdr} es cargado; por defecto se carga \enc{sólo}{solo} el paquete \code{car}. Otros paquetes requeridos \enc{serán}{seran} cargados cuando se necesiten. Si esto \enc{está}{esta} disponible, el paquete \code{car} \enc{será}{sera} cargado cuando Commander se inicie en cualquier caso.} 

\item{\code{log.commands}}{Si es \code{TRUE} (por defecto), los comandos son repetidos en la ventana de instrucciones; si es \code{FALSE}, la ventana de instrucciones no se muestra.} 

\item{\code{log.font.size}}{Tamaño de la fuente, en puntos, que es usado en la ventana de instrucciones, en la ventana de resultados, en \enc{diálogos}{dialogos} recodificados y en expresiones de cáculo, esto es, donde es usada una fuente monoespacio. Por defecto es 10 para sistemas Windows y 12 para otros sistemas.} 

\item{\code{log.height}}{La altura de la ventana de instrucciones, en \enc{líneas}{lineas}. Por defecto es 10. Estableciendo log.height a 0 tiene el mismo efecto que establer \code{log.commands} a \code{FALSE}.} 

\item{\code{log.text.color}}{Color del texto de la ventana de instrucciones; por defecto es \code{"black"}.} 

\item{\code{log.width} }{La anchura de la ventana de instrucciones y la de salida, en caracteres. Por defecto es 80.} 

\item{\code{multiple.select.mode}}{Afecta a la forma en la que múltiples variables son seleccionadas en una caja de listas de variables. Si se establece a \code{"extended"} (por defecto), el \enc{botón}{boton} izquierdo en una variable selecciona \enc{ésta}{esta} y deselecciona cualquier otra variable que estuviera seleccionada; \code{Control+botón izquierdo} acciona la selección (y puede ser usado para seleccionar variables adicionales); \code{Mayúsculas+botón izquierdo} extiende la selección. \enc{éste}{este} es el convenio \enc{está}{esta}ndar de Windows. Si lo establece a \code{"multiple"}, el \enc{botón}{boton} izquierdo acciona la selección de una variable y puede ser usado para seleccionar \enc{más}{mas} de una variable. \enc{éste}{este} es el comportamiento de Rcmdr antes de la versión 1.9-10.} 

\item{\code{output.height}}{Altura de la ventana de resultados, en \enc{líneas}{lineas}. Por defecto es dos veces la altura de la ventana de instrucciones o 20 si la ventana de instrucciones es suprimida. Establecer \code{output.height} a 0 tiene el mismo efecto que \code{console.output} a \code{TRUE}.} 

\item{\code{output.text.color}}{Color de la salida en la ventana de resultados, por defecto es \code{"blue"}.} 

\item{\code{placement}}{Emplazamiento de la ventana de R Commander, en píxeles; por defecto es \code{"$-40+20$"}, lo que pone la ventana cerca de la esquina superior derecha de la pantalla.}

\item{\code{plugins}}{Vector de caracteres con los nombres de paquetes de plugins de Rcmdr a cargar cuando Commander arranque. Los paquetes plugins \enc{también}{tambien} pueden ser cargados desde el \enc{menú}{menu} \code{Herramientas -> Cargar paquete(s)}.} 

\item{\code{suppress.menus}}{Si es \code{TRUE}, la barra de \enc{menús}{menus} y de herramientas de R Commander son suprimidas, permitiendo que otro programa (como Excel) asuma esas funciones. Por defecto (por supuesto) es \code{FALSE}.} 

\item{\code{suppress.X11.warnings}}{En (algunos) sistemas Linux X11 se generan \enc{múltiples}{multiples} advertencias por las \enc{órdenes}{ordenes} de Rcmdr, \enc{después}{despues} de abrir la ventana del dispositivo \enc{gráfico}{grafico}. 
Establecer esta \enc{opción}{opcion} a \code{TRUE} (por defecto cuando arranca interactivamente bajo X11 antes de la \enc{versión}{version} de R 2.4.0) suprime la \enc{aparición}{aparicion} de estas advertencias. Un efecto secundario indeseable es que entonces todas las advertencias y mensajes de error son interceptados por Rcmdr, incluso para las instrucciones introducidas en los avisos de R. Los mensajes producidos por tales \enc{órdenes}{ordenes} \enc{serán}{seran} impresos en la ventana de mensajes de R Commander \enc{después}{despues} de la siguiente orden generada en Rcmdr. Algunas advertencias de X11 puede ser impresas al salir de R Commander. Este problema \enc{sólo}{solo} se aplica a versiones de R anteriores a 2.4.0 y el valor por defecto de la \enc{opción}{opcion} es establecido por consiguiente.} 

\item{\code{retain.messages}}{Si es \code{TRUE} (por defecto \code{FALSE}), el contenido de la ventana de mensajes no es borrado entre mensajes. En cualquier caso, un mensaje "NOTE" no borrará un anterior "WARNING" o "ERROR".} 

\item{\code{RExcelSupport}}{Establecido como \code{TRUE} (por defecto es \code{FALSE}), los \enc{menús}{menus} y salidas son dirigidas a Excel.} 

\item{\code{scale.factor}}{Factor de escala aplicado a todos los elementos Tk, como las fuentes. Esto funciona bien \enc{sólo}{solo} en Windows. Por defecto es \code{NULL}.} 

\item{\code{showData.threshold}}{Si el \enc{número}{numero} de variables en el conjunto de datos activos excede este valor (por defecto, 100), entonces \code{edit()}, \enc{más}{mas} que \code{showData()}, es utilizado para exhibir el conjunto de datos. Un inconveniente es que el control no se devuelve a Commander hasta que la ventana de \enc{edición}{edicion} sea cerrada. 
La \enc{razón}{razon} de esta \enc{opción}{opcion} es que \code{showData()} es muy lento cuando el \enc{número}{numero} de variables es grande; fijando el umbral a 0 suprime el uso en conjunto de showData.} 

\item{\code{show.edit.button}}{Fijar a \code{TRUE} (por defecto) si quiere un \enc{botón}{boton} \code{Editar} en la ventana de Commander, que permita editar el conjunto activo de datos. Los usuarios de Windows pueden desear establecer esta \enc{opción}{opcion} a \code{FALSE} para suprimir el \enc{botón}{boton} \code{Editar} porque cambiando los nombres de las variables en el editor de datos se puede causar que R falle (aunque este problema se cree solucionado).} 

\item{\code{sort.names}}{Fijar a \code{TRUE} (por defecto) si se quiere ordenar \enc{alfabéticamente}{alfabeticamente} el nombre de las variables en una lista de variables.} 

\item{\code{tkwait}}{Esta \enc{opción}{opcion} trata un problema que, en mi
  conocimiento, es raro y puede ocurrir en algunos sistemas no
  Windows. Si R Commander causa que se cuelgue R, entonces establezca la
  \enc{opción}{opcion} \code{tkwait} a \code{TRUE}; o conserve la \enc{opción}{opcion} en
  \code{FALSE} e ignórelo. Un indeseable efecto secundario de establecer
  la \enc{opción}{opcion} \code{tkwait} a \code{TRUE} es que el aviso de \enc{órdenes}{ordenes} de la
  \enc{sesión}{sesion} de R es suprimido hasta salir de R Commander. 
Uno sin embargo \enc{todavía}{todavia} puede introducir \enc{órdenes}{ordenes} por la ventana de instrucciones. En
  particular, no hay \enc{razón}{razon} para usar esta \enc{opción}{opcion} bajo Windows y no se
  \enc{debería}{deberia} usar con la GUI de R en Windows con salida protegida cuando la
  salida \enc{esté}{este} dirigida a
  la consola de R.} 

\item{\code{use.rgl}}{Si es \code{TRUE} (por defecto), el paquete \code{rgl} \enc{será}{sera} cargado si \enc{está}{esta} presente en una librería accesible, si es \code{FALSE}, el paquete \code{rgl} \enc{será}{sera} ignorado aunque \enc{esté}{este} disponible. El paquete rgl puede a veces causar problemas cuando se arranca R bajo X11.} 

\item{\code{warning.text.color}}{Color de los mensajes de advertencia;
  por defecto es \code{"darkgreen"}.}
}

Muchas opciones pueden \enc{también}{tambien} ser establecidas mediante el \enc{menú}{menu} \emph{Archivo -> Opciones}, que \enc{reiniciará}{reiniciara} R Commander \enc{después}{despues} de que las opciones sean establecidas.

Si quiere lanzar R Commander cuando inicie R, puede incluir la siguiente
\enc{instrucción}{instruccion} en uno de los ficheros de inicio de R (por ejemplo, en el
fichero \code{Rprofile.site} de la carpeta \code{etc} de R):

\preformatted{
local({
old <- getOption("defaultPackages")
options(defaultPackages = c(old, "Rcmdr"))
})
}

Las opciones de R Commander puede ser establecidas de forma permanente
de la misma forma. Para \enc{más}{mas} \enc{información}{informacion} sobre el inicio de R, \enc{véase}{vease} \code{?Startup}.
}

\section{Avisos}{
La ventana de instrucciones de R Commander no proporciona una verdadera consola a R y tiene ciertas limitaciones. No se recomienda usar R Commander para la \enc{programación}{programacion} \enc{sería}{seria} o el \enc{análisis}{analisis} de datos que confíe primordialmente en instrucciones - usar un editor de \enc{programación}{programacion} en su lugar. 
Por ejemplo, para declaraciones de composiciones de R incluidas entre llaves \code{"\{ \}"}, incluyendo \enc{definición}{definicion} de funciones, no \enc{serían}{serian} analizadas ni ejecutadas correctamente, aunque si las \enc{líneas}{lineas} \enc{después}{despues} de las primeras que estén identandas. Puede ejecutar declaraciones de composiciones desde la ventana de instrucciones separando los comandos dentro de las llaves por puntos y comas. 
}

\section{Problemas Conocidos}{Ocasionalmente, bajo Windows, \enc{después}{despues} de teclear algún texto en un cuadro de \enc{diálogo}{dialogo} (ej. subconjunto de expresiones en el \enc{diálogo}{dialogo} de subconjunto de conjunto de datos), algunos botones en el \enc{diálogo}{dialogo} (ej. el \enc{botón}{boton} Aceptar) pueden no tener efecto cuando sean presionados. Pulsando en cualquier sitio, dentro o fuera del cuadro de \enc{diálogo}{dialogo}, \enc{debería}{deberia} restaurarse las funciones de los botones. Por lo que se ha podido comprobar, \enc{éste}{este} es un problema con Tcl/Tk de Windows.} 

\note{Esta \enc{versión}{version} debe ser compatible con SciViews, que actualmente
  \enc{sólo}{solo} funciona bajo sistemas Windows: \url{http://www.sciviews.org/SciViews-R}; mirar \code{Rcmdr.sciviews-specific}. Bajo Windows, el paquete \code{Rcmdr} puede \enc{también}{tambien} funcionar bajo de Rgui en modo SDI (interfaz de único documento) o bajo \code{rterm.exe}; puede ser que experimente problemas ejecutando \code{Rcmdr} bajo \code{ESS} con NTEmacs o XEmacs. 
}

\author{John Fox \email{jfox@mcmaster.ca} (de la \enc{versión}{version} inglesa) 

Manuel \enc{González}{Gonzalez} (traductor) \email{gonzalezperezmanuel@gmail.com}

Manuel \enc{Muñoz}{Munoz} \enc{Márquez}{Marquez} (traductor--revisor) \email{manuel.munoz@uca.es}

Véase \url{http://knuth.uca.es/R/doku.php?id=equipotraduccion}

La \enc{última}{ultima} \enc{versión}{version} de este fichero la puede encontrar en \url{http://knuth.uca.es/repos/R-contribuciones}
}

\seealso{\link{Plugins}} 

\examples{options(Rcmdr=list(log.font.size=12, contrasts=c("contr.Sum", "contr.poly")))} 

\keyword{misc}

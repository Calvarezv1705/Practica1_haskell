import System.IO 
import Data.List  

-- Definicion de tipo de datos de los Articulos
data Articulo = Articulo {nombre::String, categoria::String, cantidad::Int} deriving (Show)

-- Funcion para registrar la entrada de un articulo
registrarEntrada :: [Articulo] -> IO [Articulo]
registrarEntrada menu = do
    putStrLn "Ingrese el nombre del articulo"
    nombre <- getLine
    putStrLn "Ingrese la categoria del articulo"
    categoria <- getLine
    putStrLn "Ingrese la cantidad del articulo"
    cantidad <- getLine
    putStrLn ("Articulo " ++ nombre ++ " registrado en el inventario")
    let cantidadInt = read cantidad :: Int
    let nuevoArticulo = Articulo {nombre = nombre, categoria = categoria, cantidad = cantidadInt}
    return (nuevoArticulo:menu)

-- Funcion para guardar la informacion de los articulos en inventario.txt
guardarArticulos :: [Articulo] -> IO ()
guardarArticulos articulo = do
    writeFile "inventario.txt" (unlines (map mostrarArticulo articulo))
    putStrLn "Articulos guardados en inventario.txt"

-- Función para mostrar la información de un articulo como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria cantidad) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = " ++ categoria ++ ", cantidad = " ++ show cantidad ++ "}"

-- Funcion para parsear una linea de texto y convertirla en un articulo
parsearArticulo :: String -> Articulo
parsearArticulo linea =
    let nombre = extraerValor "nombre = " linea
        categoria = extraerValor "categoria = " linea
        cantidadStr = extraerValor "cantidad = " linea
        cantidad = read cantidadStr :: Int
    in Articulo nombre categoria cantidad

-- Funcion auxiliar para extraer valores de las cadenas de texto
extraerValor :: String -> String -> String
extraerValor campo linea = 
    let parte = dropWhile (/= '=') (dropWhile (/= campo !! 0) linea)
    in takeWhile (/= ',') (drop 2 parte)

-- Funcion para cargar la informacion de los articulos en inventario.txt
cargarArticulos :: IO [Articulo]
cargarArticulos = do
    contenido <- readFile "inventario.txt"
    let lineas = lines contenido
    return (map parsearArticulo lineas)

-- Funcion para buscar articulos por categoria
buscarPorCategoria :: [Articulo] -> IO ()
buscarPorCategoria menu = do
    putStrLn "Ingrese la categoria a buscar"
    categoriaBuscada <- getLine
    let resultados = filter (\articulo -> categoria articulo == categoriaBuscada) menu
    if null resultados
        then putStrLn "No se encontraron articulos en esta categoria."
        else mapM_ (putStrLn . mostrarArticulo) resultados

-- Funcion para listar todos los articulos
listarTodos :: [Articulo] -> IO ()
listarTodos menu = do
    putStrLn "Listado de todos los articulos:"
    mapM_ (putStrLn . mostrarArticulo) menu

-- Funcion para mostrar la cantidad de articulos por categoria
mostrarCantidadPorCategoria :: [Articulo] -> IO ()
mostrarCantidadPorCategoria menu = do
    let categorias = nub (map categoria menu)
    mapM_ (\cat -> putStrLn $ cat ++ ": " ++ show (sumarPorCategoria cat menu) ++ " artículos") categorias

-- Funcion auxiliar para sumar la cantidad de articulos por categoria
sumarPorCategoria :: String -> [Articulo] -> Int
sumarPorCategoria cat menu = sum (map cantidad (filter (\art -> categoria art == cat) menu))

-- Funcion principal del programa
main :: IO()
main = do
    putStrLn "¡Bienvenido al sistema de gestion de inventario!"
    cicloPrincipal []

-- Ciclo principal del menú del programa
cicloPrincipal :: [Articulo] -> IO()
cicloPrincipal menu = do
    putStrLn "Seleccione una opcion"
    putStrLn "1. Registrar entrada de articulo"
    putStrLn "2. Buscar articulos por categoria"
    putStrLn "3. Listar todos los articulos"
    putStrLn "4. Mostrar cantidad de articulos por categoria"
    putStrLn "5. Salir"
    op <- getLine
    case op of
        "1" -> do
            menu <- registrarEntrada menu
            guardarArticulos menu
            cicloPrincipal menu
        "2" -> do
            buscarPorCategoria menu
            cicloPrincipal menu
        "3" -> do
            listarTodos menu
            cicloPrincipal menu
        "4" -> do
            mostrarCantidadPorCategoria menu
            cicloPrincipal menu
        "5" -> putStrLn "Muchas gracias por usar nuestro programa, ¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida, por favor, seleccione una opción válida"
            cicloPrincipal menu

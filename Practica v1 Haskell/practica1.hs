import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)

-- Definición del tipo de datos para representar la información de un artículo
data Articulo = Articulo {
    nombre :: String,
    categoria :: String,
    cantidad :: Int
} deriving (Show, Read)

-- Función para ingresar un artículo al inventario
ingresarArticulo :: String -> String -> Int -> [Articulo] -> [Articulo]
ingresarArticulo nombreArticulo categoriaArticulo cantidadArticulo inventario =
    Articulo nombreArticulo categoriaArticulo cantidadArticulo : inventario

-- Función para buscar artículos por su categoría en el inventario
buscarArticulosPorCategoria :: String -> [Articulo] -> [Articulo]
buscarArticulosPorCategoria categoriaArticulo inventario =
    filter (\a -> categoriaArticulo == categoria a) inventario

-- Función para guardar la información de los artículos en un archivo de texto
guardarInventario :: [Articulo] -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo inventario))
    putStrLn "Inventario guardado en el archivo inventario.txt."

-- Función para cargar la información de los artículos desde un archivo de texto
cargarInventario :: IO [Articulo]
cargarInventario = do
    existe <- doesFileExist "inventario.txt"
    if not existe
        then do
            writeFile "inventario.txt" ""  -- Crear el archivo si no existe
            return []
        else do
            contenido <- withFile "inventario.txt" ReadMode $ \h -> do
                contenido <- hGetContents h
                contenido `deepseq` return contenido
            let lineas = lines contenido
            return (map leerArticulo lineas)
    where
        leerArticulo linea = read linea :: Articulo

-- Función para mostrar la información de un artículo como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria cantidad) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\", cantidad = " ++ show cantidad ++ "}"

-- Función para listar los artículos en el inventario
listarArticulos :: [Articulo] -> IO ()
listarArticulos [] = putStrLn "No hay artículos en el inventario."
listarArticulos articulos = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarArticulo) articulos

-- Función para mostrar la cantidad de cada artículo en el inventario
mostrarCantidadArticulos :: [Articulo] -> IO ()
mostrarCantidadArticulos [] = putStrLn "No hay artículos en el inventario."
mostrarCantidadArticulos articulos = do
    putStrLn "Cantidad de cada artículo en el inventario:"
    mapM_ (\(Articulo nombre _ cantidad) -> putStrLn $ nombre ++ ": " ++ show cantidad) articulos

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el inventario desde el archivo de texto
    inventario <- cargarInventario
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"

    -- Ciclo principal del programa
    cicloPrincipal inventario

-- Función para el ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Ingresar artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar artículos"
    putStrLn "4. Mostrar cantidad de cada artículo"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoría del artículo:"
            categoriaArticulo <- getLine
            putStrLn "Ingrese la cantidad del artículo:"
            cantidadArticulo <- readLn
            let inventarioActualizado = ingresarArticulo nombreArticulo categoriaArticulo cantidadArticulo inventario
            putStrLn $ "Artículo " ++ nombreArticulo ++ " ingresado al inventario."
            guardarInventario inventarioActualizado
            cicloPrincipal inventarioActualizado

        "2" -> do
            putStrLn "Ingrese la categoría de los artículos a buscar:"
            categoriaArticulo <- getLine
            let articulosEncontrados = buscarArticulosPorCategoria categoriaArticulo inventario
            if null articulosEncontrados
                then putStrLn "No se encontraron artículos en esa categoría."
                else do
                    putStrLn "Artículos encontrados:"
                    mapM_ (putStrLn . mostrarArticulo) articulosEncontrados
            cicloPrincipal inventario

        "3" -> do
            listarArticulos inventario
            cicloPrincipal inventario

        "4" -> do
            mostrarCantidadArticulos inventario
            cicloPrincipal inventario

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario
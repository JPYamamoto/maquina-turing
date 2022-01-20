# Máquina Universal de Turing

Simula una máquina universal de Turing, que recibe la especificación de una máquina en formato JSON y su entrada, y los ejecuta.

Al finalizar la ejecución (si esta termina), decide si la cadena fue aceptada o no.

## Instalación
El proyecto está programado en Haskell, utilizando la herramienta `Stack`.

- Para compilar el proyecto, basta clonar este repositorio y ejecutar `stack build`.
- Para correr el proyecto sin instalar el binario compilado, ejecutar `stack run`.
  - Para pasar banderas al programa usando el comando anterior, hay que agregarlas posterior a `--`. Ejemplo: `stack run -- --file test.json`.
- Para instalar el proyecto en el PATH del sistema, ejecutar `stack install`.

## Banderas
Es posible pasar banderas al programa, aunque no necesario.

- `-h | --help`: Imprime la ayuda y una descripción detallada de las banderas disponibles.
- `-f FILE | --file FILE`: Ingresa la ruta `FILE` del archivo JSON con la especificación de la máquina.
- `-i INPUT | --input INPUT`: Ingresa la cadena que será usada como entrada para la máquina.

*NOTA:* Si el programa no recibe todas las banderas, pedirá al usuario ingresar la información faltante a través de la entrada estándar.

## Ejemplo
Al ejecutar `stack run -- -f test.json -i ababab` podemos ver la ejecución de una máquina que acepta cadenas de la forma `{xxx | x ∈ {a,b}*}`. En particular, va a aceptar a la cadena `ababab`.

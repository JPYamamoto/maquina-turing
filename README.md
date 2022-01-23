# Máquina Universal de Turing

Simula una máquina universal de Turing, que recibe la especificación de una máquina en formato JSON y su entrada, y los ejecuta.

Al finalizar la ejecución (si esta termina), decide si la cadena fue aceptada o no.

## Instalación
El proyecto está programado en Haskell, utilizando la herramienta [`Stack`](https://docs.haskellstack.org/en/stable/README/).

> *Nota:* Si `Stack` no se encuentra actualizado, puede lanzar una excepción de tipo `Error in $.resolver`.
> Si eso sucede, ejecutar `stack upgrade` soluciona el problema.

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

## Máquinas Ejemplo
En el directorio `programas/` incluimos dos máquinas que se pueden utilizar como ejemplo.

- `ejercicio2.json`: Es una máquina que acepta las cadenas de la forma `0^n 1^n 2^n` con `n>=1`.
- `test.json`: Es una máquina que acepta las cadenas de la forma `{xxx | x ∈ {a,b}*}`.

Al ejecutar `stack run -- -f programas/ejercicio2.json -i 000111222` podemos ver la ejecución de la máquina `ejercicio2.json`, y vemos cómo acepta la cadena `000111222`.

También es posible ejecutar las máquinas sin pasar los argumentos como banderas. Al ejecutar `stack run`, el programa pedirá al usuario ingresar el nombre del archivo JSON con la máquina a ejecutar, por ejemplo `programas/test.json`. Luego pedirá al usuario ingresar la cadena a procesar, por ejemplo `abaabaaba`, y veremos que la cadena es aceptada.

# Guía de contribución

## Flujo de trabajo con Git (protocolo del equipo)

1. **Al empezar:** en RStudio, pestaña Git, **Pull** antes de abrir cualquier archivo.
2. **Mientras trabaja:** guarde con Ctrl+S; Git no guarda por usted.
3. **Antes de subir:** ejecute la QA completa y no suba si no da **APTO**:
   ```bash
   Rscript scripts/qa_all.R
   ```
4. **Al terminar (aunque el trabajo esté incompleto):** *Commit* de todos los archivos modificados
   con un mensaje descriptivo de una línea, **Pull** otra vez y luego **Push**.
5. **Si hay un conflicto:** deténgase y avise a la coordinación del proyecto. No lo resuelva por su cuenta.

Nunca haga *push* sin haber hecho *pull* antes, ni termine una sesión sin subir.

## Mensajes de commit

Una línea que diga qué se hizo. Aceptables: `Generalizar coordenadas a celdas de 0,5° en el ETL`,
`Corregir filtro de años cuando el rango es completo`. **No** aceptables: `changes`, `update`, `arreglos`.

## Reglas de código (las verifica `scripts/qa_all.R`)

- **MVC:** el Modelo (`R/model`) no usa `shiny`; la Vista (`R/view`) no accede a datos ni seguridad;
  el Controlador (`R/controller`) los une.
- **Privacidad:** ninguna coordenada exacta fuera del ETL y del SIG (`R/model/etl.R`, `geo.R`).
- **Sin descargas**, sin `HTML()` con datos de usuario, sin secretos ni rutas absolutas en el código.
- **Estilo:** `styler` (tidyverse) y `lintr` sin avisos; líneas de hasta 100 caracteres.
- **Documentación:** toda función de nivel superior lleva un bloque roxygen (`#'`) con `@param` y `@return`
  (los ayudantes internos pueden llevar un comentario breve).
- **Tamaño:** funciones de hasta 50 líneas.
- **Errores:** nunca se silencian con `tryCatch(..., error = function(e) NULL)`.
- **Todo defecto S1/S2 corregido deja una prueba nueva que lo reproduce.**

## Comandos útiles

```bash
Rscript scripts/qa_all.R                        # compuertas de datos + estilo + pruebas
Rscript scripts/run_test_file.R test-etl.R      # una sola batería con detalle
Rscript -e "styler::style_dir('R')"             # formatear
Rscript -e "lintr::lint_dir('R')"               # revisar estilo
```

## Datos

Los datos reales van en `data/raw/` (fuera de Git y del despliegue). El repositorio debe ser **privado**.

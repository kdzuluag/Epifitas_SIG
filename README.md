# EpIG Data Visualizer — modelo base MVC en Shiny

Herramienta de **visualización y revisión** de registros de ocurrencia de epífitas vasculares del Neotrópico para **taxónomos y especialistas**: filtrar por grupo taxonómico, explorar en mapa, analizar y verificar si cada punto está bien ubicado — **sin exponer coordenadas exactas y sin descargas**.

> Los datos incluidos son **sintéticos** (nombres de especies inventados) y sirven para probar la arquitectura. Con datos reales, siga la sección "Usar datos reales".

## Inicio rápido

```bash
# 1) Dependencias (una vez)
Rscript deploy/install_deps.R

# 2) Datos de demostración + ETL (genera data/processed/*.rds)
Rscript scripts/00_make_demo_data.R
Rscript scripts/01_build_data.R

# 2b) Referencia de departamentos/estados/provincias para la verificación de ubicación
#     (una sola vez; exige internet la primera vez y ya viene generada en el repositorio)
Rscript scripts/01b_build_admin1.R

# 3) Documentación Quarto (metodología embebida en la app, arquitectura, calidad de datos)
Rscript scripts/03_render_docs.R

# 4) Crear su usuario (la contraseña se pide por pantalla; solo se guarda su hash)
Rscript scripts/04_add_user.R su_usuario admin

# 5) QA completa (compuertas de datos + pruebas) y ejecución (pedirá iniciar sesión)
Rscript scripts/qa_all.R
Rscript -e "shiny::runApp()"
```

## Arquitectura (MVC)

| Carpeta | Capa | Responsabilidad |
|---|---|---|
| `R/core` | Núcleo | Configuración por variables de entorno, esquema Darwin Core, utilidades |
| `R/model` | **Modelo** | Funciones puras sin `shiny`: SIG (`geo.R`), ETL (`etl.R`), taxonomía, estadística, seguridad, persistencia |
| `R/controller` | **Controlador** | Reactividad: filtros → datos, selección de registro, verificación de ubicación, acceso |
| `R/view` | **Vista** | Módulos de interfaz (bslib): filtros, resumen, análisis, mapa, tabla, detalle, acerca de |
| `scripts/` | Ingeniería de datos y QA | Datos demo, ETL, compuertas, estilo/lint, QA completa, render de documentos |
| `docs/` | Quarto | `metodologia`, `arquitectura`, `calidad_datos`, `qa` |
| `deploy/` | Despliegue | Posit Connect, Docker, nginx (**plantillas no probadas**) |

Detalle, diagramas, decisiones y modelo de amenazas: `docs/out/arquitectura.html`.

## Qué resuelve, por disciplina

- **Privacidad / ciberseguridad:** la generalización ocurre en el ETL; la app **no recibe coordenadas exactas** (`assert_no_raw_coordinates` detiene el arranque si aparecen). Sesiones sin identidad ⇒ cero datos. Escape de salida, SQL parametrizado, límite de tasa, bitácora de auditoría, sin descargas.
- **SIG:** WGS 84, cuadrícula de generalización (0,5° ≈ 55 km; 2× y 4× para especies sensibles), capa de riqueza por celda, máscara biorregional, geometrías simplificadas.
- **Taxonomía y botánica:** niveles orden/familia/género/especie con cursivas correctas para género y especie, búsqueda insensible a tildes, pisos altitudinales neotropicales, alertas (género ≠ epíteto, familia atípica para el género, taxonomía incompleta).
- **Estadística:** curva de acumulación por permutación y Chao1 con corrección de sesgo, con avisos de interpretación (registro ≠ individuo).
- **Ingeniería de datos:** ETL determinista y validado, clave única por registro, interpretación de elevación/fecha heterogéneas, reporte de calidad en Quarto.
- **Optimización:** `.rds` precalculado, índice de búsqueda precomputado, carga única por proceso, `bindCache`, selectize/DT en servidor, `leafletProxy` + agrupación.
- **Diseño:** identidad visual propia (paleta índigo, ámbar y ciruela; sin colores, logotipos ni textos de otras instituciones), cabecera de grupo, cajas de indicadores, pestañas en mayúsculas, pestaña **Especies** (lista buscable + ficha) y pie institucional. Aún no hay cuentas de usuario ni validación por expertos.
- **UX/UI:** bslib (Bootstrap 5), paleta apta para daltonismo (Okabe‑Ito/Tol), etiquetas accesibles, textos alternativos en gráficos, foco visible, `prefers-reduced-motion`, diseño responsivo.

## Calidad (QA) y buenas prácticas

`Rscript scripts/qa_all.R` decide si el código y los datos están listos (código de salida ≠ 0 si no).
Resultado actual: **106 pruebas, 414 expectativas, 11/11 compuertas críticas de datos, `lintr` con 0
avisos y 98,7 % de cobertura de líneas** (piso exigido: 90 %).

| Nivel | Qué protege |
|---|---|
| Compuertas de datos (`R/model/qa.R`, `scripts/02_qa_data.R`) | Sin coordenadas exactas, puntos dentro de su celda, celdas más gruesas para especies sensibles, claves únicas, rangos válidos. **Bloquean el despliegue** |
| Estilo (`lintr` + `styler`, `.lintr`) | Estilo tidyverse, líneas de hasta 100 caracteres, nombres coherentes |
| Unitarias e integración (`tests/testthat`) | Modelo, controladores y módulos de la Vista (`shiny::testServer`), datos crudos ⇄ procesados |
| Guardias de regresión y de código | Los 7 defectos del 04/08/2026, sin descargas, MVC, sin `HTML()` crudo, sin secretos |
| Calidad de código (`test-code-quality.R`) | Toda función documentada (roxygen con `@param` y `@return`), funciones de ≤ 50 líneas, sin `<<-` ni errores silenciosos, dependencias declaradas, finales de línea LF |
| Contrato de interfaz y rendimiento | Idioma, etiquetas, texto alternativo; presupuestos de tiempo y tamaño |
| Cobertura (`covr`) | Líneas ejecutadas por las pruebas (mide ejecución, no corrección) |

Las guardias se validaron con pruebas de mutación (se inyectaron violaciones a propósito y fueron
detectadas). Criterios de aceptación, lista manual, accesibilidad y política de defectos:
`docs/out/qa.html`. Reglas de contribución y flujo Git del equipo: `CONTRIBUTING.md`.
Reproducibilidad: `DESCRIPTION` declara las dependencias (fuente de verdad para instalarlas,
incluido el despliegue). `renv.lock` fija versiones exactas para uso local (`renv::restore()`,
regenerar con `scripts/snapshot_deps.R`); **no se versiona a propósito**, porque `rsconnect` lo
detecta y trata de usarlo para instalar paquetes en el servidor en vez de escanear el código,
lo que rompe el despliegue si la librería local no coincide exactamente con el archivo.
`DESCRIPTION` **no** lleva `Type: Package` a propósito, porque Shiny lo interpretaría como un
paquete de R. CI de plantilla en `.github/workflows/qa.yml` (**no ejecutado aún**).

## Usar datos reales

1. Coloque el Excel (20 columnas Darwin Core) en `data/raw/`. El polígono del Neotrópico ya está en `data/raw/morrone2014/` (Morrone 2014, en *shapefile* de Löwenberg-Neto 2014, CC BY 4.0; ver `docs/metodologia.qmd`). **No despliegue `data/raw/`.**
2. Opcional: añada la columna `sensitivityLevel` (0/1/2) para usar celdas más gruesas en especies sensibles.
3. `Rscript scripts/01_build_data.R` (variables opcionales: `EPIG_RAW_FILE`, `EPIG_RAW_SHAPE`, `EPIG_GRID_DEG`).
4. Revise `docs/out/calidad_datos.html` y ajuste reglas de calidad.

## Configuración (variables de entorno)

| Variable | Valor | Descripción |
|---|---|---|
| `EPIG_ENV` | `development` / `production` | En producción `auth_mode=none` está prohibido |
| `EPIG_AUTH_MODE` | `local` (por defecto) / `connect` / `proxy` / `none` | Modo de autenticación. `local` = usuario y contraseña; `none` solo en desarrollo |
| `EPIG_USERS_DB` | ruta | Base de usuarios (hashes de contraseña), por defecto `data/private/users.sqlite` |
| `EPIG_LOGIN_MAX_ATTEMPTS`, `EPIG_LOGIN_WINDOW` | número | Intentos fallidos permitidos por sesión (5) y ventana en segundos (300) |
| `EPIG_ADMIN_GROUPS`, `EPIG_ADMIN_USERS`, `EPIG_ALLOWED_GROUPS` | lista separada por comas | Roles y grupos permitidos |
| `EPIG_PROXY_TOKEN` | secreto | Obligatorio en modo `proxy` |
| `EPIG_VERIFICATION_DB`, `EPIG_AUDIT_LOG` | rutas | Estado escrito por la app (fuera del bundle) |
| `EPIG_DETAIL_MAX_CHARS`, `EPIG_IDENTITY_MAX_CHARS` | número | Longitud máxima del detalle libre (500) y de nombre/profesión/departamento (150) |
| `EPIG_VERIFICATION_RATE_MAX`, `EPIG_VERIFICATION_RATE_WINDOW` | número | Verificaciones permitidas por sesión (5) y ventana en segundos (60) |

## Despliegue

- **Posit Connect (plan con autenticación):** `EPIG_AUTH_MODE=connect`, defina grupos en Connect y ejecute `deploy/deploy_connect.R`. El disco es efímero: para verificaciones persistentes use una BD externa (solo cambia `vs_open()` en `R/model/verification_store.R`).
- **Auto‑hospedaje:** `docker build -f deploy/Dockerfile -t epig .` detrás de nginx + oauth2‑proxy (`deploy/nginx.conf`), `EPIG_AUTH_MODE=proxy`.
- **Advertencia:** `www/` se sirve sin autenticación por Shiny; solo contiene CSS y la metodología (sin datos). No coloque allí nada sensible.

## Límites conocidos

- Sin datos reales ni validación contra un *backbone* taxonómico (World Flora Online / World Ferns).
- Plantillas de `deploy/` no probadas en este entorno.
- Sin pruebas automatizadas de interfaz de navegador (`shinytest2`); la UI se verificó manualmente. Sin pruebas de carga. Accesibilidad con lector de pantalla y teclado: pendiente (ver `docs/qa.qmd`).
- Un usuario autenticado puede leer la tabla página a página; se mitiga con auditoría, no se elimina.
- Interfaz solo en español (internacionalización pendiente).

## Flujo de trabajo Git (protocolo del equipo)

Pull antes de empezar → guardar seguido → Commit con mensaje descriptivo → Pull → Push al terminar. Ante un conflicto, detenerse y contactar a la coordinación del proyecto.

## Licencia

Repositorio privado, sin licencia de código abierto: todos los derechos reservados. Nadie fuera
del equipo puede copiar, modificar ni redistribuir este código sin autorización explícita de la
coordinación del proyecto. Se eligió así porque el grupo aún evalúa cobrar por el acceso a la
plataforma; si más adelante se decide abrir el código, se puede añadir una licencia entonces.

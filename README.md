# Monitor de Proyectos RIGI

Este repositorio contiene un sitio web reproducible en **R + Quarto** para monitorear proyectos del RIGI a partir del archivo:

```text
RIGI_tracker_data_final_con_proyectos_integrados.xlsx
```

El informe usa exclusivamente la solapa:

```text
Proyectos
```

No lee ni procesa otras solapas del archivo.

## Estructura del proyecto

```text
rigi-dashboard/
│
├── data/
│   └── RIGI_tracker_data_final_con_proyectos_integrados.xlsx
│
├── R/
│   ├── 00_packages.R
│   ├── 01_load_data.R
│   ├── 02_clean_data.R
│   ├── 03_indicators.R
│   └── 04_plots.R
│
├── index.qmd
├── presentation.qmd
├── _quarto.yml
├── styles.css
├── README.md
└── .github/
    └── workflows/
        └── render.yml
```

## Qué hace cada archivo

- `data/RIGI_tracker_data_final_con_proyectos_integrados.xlsx`: archivo fuente. Debe contener la solapa `Proyectos`.
- `R/00_packages.R`: carga e instala, si hace falta, los paquetes de R necesarios.
- `R/01_load_data.R`: define la ruta del Excel y lee exclusivamente la solapa `Proyectos`.
- `R/02_clean_data.R`: limpia nombres de columnas, convierte montos, parsea fechas y crea variables auxiliares.
- `R/03_indicators.R`: calcula KPIs, rankings, tablas auxiliares y resumen ejecutivo automático.
- `R/04_plots.R`: define visualizaciones interactivas con `ggplot2`, `plotly` y `DT`.
- `index.qmd`: página web principal del informe.
- `presentation.qmd`: presentación ejecutiva en formato Quarto RevealJS.
- `_quarto.yml`: configuración general del sitio web.
- `styles.css`: estilo visual del dashboard.
- `.github/workflows/render.yml`: workflow para renderizar y publicar automáticamente en GitHub Pages.

## Cómo correrlo localmente

1. Instalar R.
2. Instalar Quarto.
3. Abrir una terminal en la carpeta del proyecto.
4. Ejecutar:

```bash
quarto render
```

5. Abrir el archivo generado dentro de `_site/index.html`.

También podés previsualizarlo con:

```bash
quarto preview
```

## Cómo actualizar el informe

1. Reemplazar el Excel en la carpeta `data/`.
2. Verificar que la solapa se siga llamando `Proyectos`.
3. Ejecutar:

```bash
quarto render
```

4. Subir los cambios a GitHub:

```bash
git add .
git commit -m "Actualizar dashboard RIGI"
git push
```

5. GitHub Pages actualizará el sitio si está configurado con el workflow incluido.

## Publicación en GitHub Pages

### Alternativa A: render manual

Renderizar localmente y subir los archivos:

```bash
quarto render
git add .
git commit -m "Actualizar dashboard RIGI"
git push
```

### Alternativa B: GitHub Actions

El repositorio incluye un workflow en:

```text
.github/workflows/render.yml
```

Para usarlo:

1. Subí este proyecto a GitHub.
2. Entrá en `Settings > Pages`.
3. En `Build and deployment`, elegí `GitHub Actions`.
4. Hacé un push a `main` o `master`.
5. El workflow va a instalar R, instalar Quarto, instalar paquetes, renderizar el sitio y publicarlo.

## Nota importante sobre datos

Si el repositorio es público, el archivo Excel dentro de `data/` también será público. Si la base contiene información sensible o de circulación restringida, conviene usar un repositorio privado o excluir el archivo fuente del repositorio público.

## Actualización: distinción entre total y aprobados

El tablero distingue explícitamente entre:

- **Universo total:** todos los proyectos presentes en la solapa `Proyectos`, cualquiera sea su estado administrativo.
- **Proyectos aprobados:** solo los registros cuyo estado administrativo se clasifica como `Aprobado`.

El informe incluye tarjetas, resumen ejecutivo, gráficos comparativos y rankings separados para ambos universos.

# Monitor de Proyectos RIGI

Sitio web reproducible construido con **R + Quarto** para monitorear proyectos del RIGI a partir de la solapa `Proyectos` del archivo:

```text
RIGI_tracker_data_final_con_proyectos_integrados.xlsx
```

## Enfoque de esta versión

La estructura del informe fue reorganizada para priorizar la lectura de los proyectos aprobados:

1. Primero se muestran los **proyectos aprobados** y sus estadísticas.
2. Luego se muestran los **proyectos pendientes de aprobación** o en evaluación.
3. Finalmente se presenta una comparación entre aprobados y pendientes, más un panorama general del universo total.

## Aclaración metodológica

Para los proyectos aprobados, se utilizó la información del Boletín Oficial y las empresas inferidas por Globaris. Para los proyectos en evaluación, se utilizó la información del dashboard de Globaris.

## Estructura

```text
rigi-dashboard/
├── data/
│   └── RIGI_tracker_data_final_con_proyectos_integrados.xlsx
├── R/
│   ├── 00_packages.R
│   ├── 01_load_data.R
│   ├── 02_clean_data.R
│   ├── 03_indicators.R
│   └── 04_plots.R
├── index.qmd
├── presentation.qmd
├── _quarto.yml
├── styles.css
├── README.md
└── .github/
    └── workflows/
        └── render.yml
```

## Correr localmente

```bash
quarto render
quarto preview
```

## Actualizar datos

1. Reemplazar el archivo dentro de `data/`.
2. Mantener el nombre `RIGI_tracker_data_final_con_proyectos_integrados.xlsx`.
3. Verificar que la solapa siga llamándose `Proyectos`.
4. Ejecutar:

```bash
quarto render
```

5. En GitHub Desktop, hacer commit y luego `Push origin`.

## Publicación

El sitio está preparado para publicarse con GitHub Pages mediante GitHub Actions.

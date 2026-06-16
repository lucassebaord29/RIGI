# Monitor de Proyectos RIGI

Sitio web reproducible construido con **R + Quarto** para monitorear proyectos del RIGI a partir de la solapa `Proyectos` del archivo:

```text
RIGI_tracker_data_final_con_proyectos_integrados.xlsx
```

## Enfoque de esta versión

La estructura del informe fue reorganizada para priorizar los proyectos aprobados:

1. Primero se muestran los **proyectos aprobados** y sus estadísticas.
2. Luego se muestran los **proyectos pendientes de aprobación** o en evaluación.
3. Finalmente se presenta una comparación entre aprobados y pendientes, más un panorama general del universo total.

## Cambios incorporados

- Se eliminó el uso de abreviaturas de billones: todos los montos se muestran en **millones de USD**.
- Se agregaron estadísticas y gráficos de **empleo directo e indirecto** para proyectos aprobados.
- Se generan archivos descargables para las bases interactivas:
  - `downloads/base_interactiva_aprobados.xlsx`
  - `downloads/base_interactiva_aprobados.csv`
  - `downloads/base_interactiva_pendientes.xlsx`
  - `downloads/base_interactiva_pendientes.csv`
- En los archivos descargables, las columnas `Monto (mill. USD)`, `Activos Computables (mill. USD)` y `Empleos (directos e indirectos)` se exportan como numéricas.
- Los proyectos con más de una provincia separada por `;` se desagregan para gráficos territoriales, asignando proporcionalmente monto, activos y empleo para evitar doble conteo.

## Aclaración metodológica

Para los proyectos aprobados, se utilizó la información del Boletín Oficial y las empresas inferidas por Globaris. Para los proyectos en evaluación, se utilizó la información del dashboard de Globaris. Los datos de empleos directos e indirectos se obtuvieron de la página web del Ministerio de Economía.

Fuentes:

- Globaris: https://app.powerbi.com/view?r=eyJrIjoiNTFjY2E4NTYtOTVlNy00YmFiLWIwYmMtNWZkMjE4OTNhYmRiIiwidCI6IjNlMDUxM2Q2LTY4ZmEtNDE2ZS04ZGUxLTZjNWNkYzMxOWZmYSIsImMiOjR9&pageName=d1ee75596a51a9bde708
- Ministerio de Economía - RIGI: https://www.argentina.gob.ar/economia/rigi

## Estructura

```text
rigi-dashboard/
├── data/
│   └── RIGI_tracker_data_final_con_proyectos_integrados.xlsx
├── downloads/
│   └── archivos generados al renderizar
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

El sitio está preparado para publicarse con GitHub Pages mediante GitHub Actions. El resultado final está pensado para publicarse en:

```text
https://lucassebaord29.github.io/RIGI/
```

# Monitor de Proyectos RIGI

Sitio web reproducible construido con **R + Quarto + GitHub Pages** a partir de la solapa `Proyectos` del archivo:

```text
RIGI_tracker_data_final_con_proyectos_integrados.xlsx
```

## Cambios de esta versión

- El informe prioriza primero los **proyectos aprobados**.
- Luego muestra los **proyectos pendientes de aprobación / en evaluación**.
- Los montos se muestran en **millones de USD**, sin abreviaturas tipo `B`.
- Se incorporan estadísticas y gráficos de **empleo directo e indirecto** para proyectos aprobados.
- Las provincias múltiples separadas por `;` se tratan mediante **asignación proporcional** de monto, activos computables y empleo para evitar doble conteo territorial.
- Se agregan descargas en `.xlsx` y `.csv` para:
  - `Base interactiva: aprobados`
  - `Base interactiva: pendientes`
- En las descargas, las columnas `Monto (mill. USD)`, `Activos Computables (mill. USD)` y `Empleos (directos e indirectos)` se mantienen como numéricas.
- Se mejoró la estética general del sitio y de los gráficos.
- Los nombres largos de proyectos, sectores o provincias en los gráficos se muestran en hasta dos renglones para mejorar la legibilidad.

## Fuentes y aclaración metodológica

Para los proyectos aprobados, se utilizó la información del Boletín Oficial y las empresas inferidas por Globaris. Para los proyectos en evaluación, se utilizó la información del dashboard de Globaris. Los datos de empleos directos e indirectos se obtuvieron de la página web del Ministerio de Economía.

## Cómo correr localmente

Desde la carpeta del proyecto:

```bash
quarto render
quarto preview
```

## Cómo actualizar el informe

1. Reemplazar el archivo Excel en `data/RIGI_tracker_data_final_con_proyectos_integrados.xlsx`.
2. Verificar que la solapa se siga llamando `Proyectos`.
3. Ejecutar:

```bash
quarto render
quarto preview
```

4. Si está correcto, subir cambios con GitHub Desktop:
   - escribir un mensaje en `Summary`;
   - tocar `Commit to main`;
   - tocar `Push origin`.

El sitio publicado debería actualizarse en:

```text
https://lucassebaord29.github.io/RIGI/
```

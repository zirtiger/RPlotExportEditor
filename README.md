# Plot Editor (WYSIWYG)

A comprehensive Shiny app for editing ggplot2 plots with real-time preview and export capabilities. This app provides a WYSIWYG (What You See Is What You Get) interface for customizing plots before export.

## Features

### 🎯 **Core Functionality**
- Load multiple ggplot2 plots (.rds files)
- Real-time preview of all changes
- Grid layout system for combining multiple plots
- Export plots in multiple formats (PNG, TIFF, PDF, SVG, EPS)

### 🎨 **Text Customization**
- **Labels**: Title, subtitle, caption, X/Y axis labels
- **Text Sizes**: Customizable sizes for all text elements
  - Title, subtitle, caption sizes
  - Axis title and text sizes
  - Legend title and text sizes
- **Axis Control**: Set min/max values and major/minor break intervals

### 🎭 **Theme Customization**
- **Base Themes**: Choose from ggplot2's built-in themes
- **Backgrounds**: Customize panel and plot backgrounds
- **Grid Lines**: Control major/minor grid lines with custom colors
- **Legend**: Position and styling options

### 📐 **Grid Layout System**
- Arrange multiple plots in custom grid layouts
- Assign specific plots to grid cells
- Unified legend management
- Grid-specific export settings

### 💾 **Export Options**
- **Multiple Formats**: PNG, TIFF, PDF, SVG, EPS
- **Quality Control**: Common DPI presets (72, 150, 300, 600) + custom options
- **Dimensions**: Precise control over width and height in millimeters
- **Download Buttons**: Direct download from both plot tabs and export panel

### 🎛️ **User Interface**
- **Tighter Layout**: Compact sidebar with full-height settings panel
- **Intuitive Controls**: Sliders for text sizes, DPI presets for common use cases
- **Organized Menus**: Logical grouping of related options
- **Responsive Design**: Adapts to different screen sizes

## Menu Structure

1. **Grid** - Layout and arrangement of multiple plots
2. **Export** - Export settings and download options
3. **Text** - Text content and sizing customization
4. **Theme** - Visual styling and appearance

## Installation & Usage

### Prerequisites
```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "patchwork"))
```

### Optional Dependencies (for enhanced export)
```r
install.packages(c("ragg", "svglite", "pdftools", "rsvg", "magick"))
```

### Running the App
```r
shiny::runApp()
```

## File Structure

```
plot-editor/
├── app.R                 # Main entry point
├── R/
│   ├── globals.R        # Default settings and constants
│   ├── ui.R            # Main UI layout
│   ├── server.R        # Server logic and download handlers
│   ├── state.R         # Reactive state management
│   ├── helpers_plots.R # Plot editing and export functions
│   ├── panes_grid.R    # Grid layout controls
│   ├── panes_export.R  # Export settings and controls
│   ├── panes_text.R    # Text customization options
│   ├── panes_theme.R   # Theme and styling options
│   └── tabs_preview.R  # Plot preview tabs
├── www/
│   └── style.css       # Custom styling
└── README.md           # This file
```

## Usage Tips

### Loading Plots
- Use the file input to load .rds files containing ggplot2 objects
- Click "Load 3 demo plots" to start with example data
- Multiple plots can be loaded simultaneously

### Grid Layout
- Set the number of rows and columns
- Assign plots to specific grid cells
- Use "Collect/common legend" for unified legends

### Export Settings
- Configure dimensions in millimeters for precise control
- Choose from common DPI presets or set custom values
- Apply export settings to all plots with one click

### Text Customization
- Edit text content and sizes independently
- Control axis limits and break intervals
- Apply text settings across all plots

### Theme Customization
- Mix and match theme elements
- Customize backgrounds and grid lines
- Control legend appearance

## Export Formats

- **PNG/TIFF**: Raster formats with DPI control
- **PDF**: Vector format, ideal for publications
- **SVG**: Web-friendly vector format
- **EPS**: PostScript format for legacy systems

## Contributing

This app is designed to be easily extensible. New customization options can be added by:
1. Adding new fields to the `BASE` configuration in `globals.R`
2. Creating UI controls in the appropriate pane file
3. Adding observers in the pane's observer function
4. Implementing the logic in `helpers_plots.R`

## License

This project is open source and available under the MIT License.

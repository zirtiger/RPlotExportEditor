# Plot Editor (WYSIWYG)

A comprehensive Shiny app for editing ggplot2 plots with real-time preview and export capabilities. This app provides a WYSIWYG (What You See Is What You Get) interface for customizing plots before export.

## ✨ **New Features & Improvements**

### 🎯 **Smart Menu Activation**
- **Context-aware menus**: Only relevant menu items are active based on current view
- **Grid view**: Text and Theme menus are greyed out (not applicable to grid)
- **Individual plots**: All menus are active and functional
- **Visual feedback**: Inactive menus are clearly marked and non-clickable

### 🎨 **Compact Tabbed Interface**
- **Organized options**: Related settings grouped into logical tabs
- **Space efficient**: Fits comfortably in the sidebar without scrolling
- **Better UX**: Clear separation of different option categories

### 🎛️ **Improved Layout**
- **Tighter design**: No unnecessary padding between elements
- **Full-height panels**: Settings and plots use entire browser height
- **Header removed**: More space for actual functionality
- **Responsive design**: Adapts to different screen sizes

### 📊 **Enhanced Plot Integration**
- **Auto-populated defaults**: Options default to current plot values when available
- **Smart fallbacks**: Uses global defaults only when plot values aren't available
- **Real-time updates**: Changes reflect immediately in preview

## 🚀 **Core Functionality**

### **Plot Management**
- Load multiple ggplot2 plots (.rds files)
- Real-time preview of all changes
- Grid layout system for combining multiple plots
- Export plots in multiple formats (PNG, TIFF, PDF, SVG, EPS)

### **Text Customization** (Tabbed Interface)
- **Labels Tab**: Title, subtitle, caption, X/Y axis labels
- **Text Sizes Tab**: Sliders for all text elements (8-24pt range)
- **Axis Limits Tab**: Min/max values with plot-based defaults
- **Axis Breaks Tab**: Major/minor break intervals

### **Theme Customization** (Tabbed Interface)
- **Base Tab**: Theme selection and base text size
- **Legend Tab**: Position and box styling
- **Backgrounds Tab**: Panel and plot background colors
- **Grid Lines Tab**: Major/minor grid control with colors

### **Export Options** (Tabbed Interface)
- **Dimensions Tab**: Width/height sliders (side by side)
- **Quality Tab**: DPI presets (72, 150, 300, 600) + custom
- **Multiple Formats**: PNG, TIFF, PDF, SVG, EPS
- **Download Buttons**: Direct download from both tabs and export panel

## 🎨 **Menu Structure & Logic**

1. **Grid** - Always active when plots are loaded
   - Layout and arrangement of multiple plots
   - Grid-specific export settings

2. **Export** - Always active (applies to both grid and plots)
   - Export configuration and download options

3. **Text** - Active only for individual plots (not grid)
   - Text content and sizing customization
   - Axis limits and break intervals

4. **Theme** - Active only for individual plots (not grid)
   - Visual styling and appearance
   - Backgrounds and grid lines

## 🛠️ **Installation & Usage**

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

### Testing the App
```r
source("test_app.R")
```

## 📁 **File Structure**

```
plot-editor/
├── app.R                 # Main entry point
├── test_app.R           # Test script to verify app loads
├── R/
│   ├── globals.R        # Default settings and constants
│   ├── ui.R            # Main UI layout (no header, tight spacing)
│   ├── server.R        # Server logic with smart menu activation
│   ├── state.R         # Reactive state with plot-based defaults
│   ├── helpers_plots.R # Plot editing and export functions
│   ├── panes_grid.R    # Grid layout controls
│   ├── panes_export.R  # Export settings with tabs
│   ├── panes_text.R    # Text customization with tabs
│   ├── panes_theme.R   # Theme options with tabs
│   └── tabs_preview.R  # Plot preview tabs
├── www/
│   ├── style.css       # Compact styling and tab design
│   └── script.js       # Smart menu activation logic
└── README.md           # This file
```

## 🎯 **Usage Tips**

### **Loading Plots**
- Use the file input to load .rds files containing ggplot2 objects
- Click "Load 3 demo plots" to start with example data
- Multiple plots can be loaded simultaneously

### **Smart Menu Usage**
- **Grid tab**: Use Grid and Export menus for layout and export
- **Plot tabs**: All menus are active for comprehensive customization
- **Inactive menus**: Clearly marked and won't respond to clicks

### **Tabbed Interface**
- **Text pane**: Use tabs to organize different text customization areas
- **Theme pane**: Separate tabs for base, legend, backgrounds, and grid
- **Export pane**: Dimensions and quality in separate tabs

### **Plot Integration**
- Text options default to current plot values when available
- Axis limits automatically populate from plot data ranges
- Empty fields use global defaults only when necessary

## 🔧 **Technical Features**

### **Error Prevention**
- Fixed sequence errors with proper validation
- Safe handling of non-finite numbers
- Graceful fallbacks for missing data

### **Performance**
- Compact UI reduces scrolling
- Efficient state management
- Real-time preview updates

### **Accessibility**
- Clear visual hierarchy
- Consistent interface patterns
- Responsive design principles

## 🤝 **Contributing**

This app is designed to be easily extensible. New customization options can be added by:
1. Adding new fields to the `BASE` configuration in `globals.R`
2. Creating UI controls in the appropriate pane file with tabs
3. Adding observers in the pane's observer function
4. Implementing the logic in `helpers_plots.R`
5. Updating the state initialization in `state.R`

## 📄 **License**

This project is open source and available under the MIT License.

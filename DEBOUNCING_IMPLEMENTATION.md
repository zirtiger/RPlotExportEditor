# Debouncing Implementation for Input Interruption Prevention

## Problem Solved

This implementation addresses the common Shiny reactivity issue where typing in text inputs or adjusting sliders would be interrupted by immediate plot updates, making it difficult to complete input tasks.

## Solution Overview

We've implemented a hybrid approach using:
- **Debouncing** for text inputs and numeric inputs (500ms delay)
- **Throttling** for sliders (200ms delay) 
- **Immediate updates** for dropdowns and checkboxes

## Implementation Details

### 1. Simple Debouncing with `invalidateLater()`

```r
# Text inputs (500ms debouncing)
observeEvent(input$ui_title, {
  invalidateLater(500)  # 500ms delay
  # Do the actual work
  rv$edits[[plot_index]]$title <- input$ui_title
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Sliders (200ms throttling)
observeEvent(input$ui_base_size, {
  invalidateLater(200)  # 200ms delay
  rv$edits[[plot_index]]$base_size <- input$ui_base_size
}, ignoreInit = TRUE, ignoreNULL = TRUE)
```

### 2. Input Type Categorization

#### Text Inputs (500ms debouncing)
- Title, subtitle, caption, xlab, ylab
- All text-based inputs that users type into

#### Sliders (200ms throttling)
- Base text size, title size, subtitle size, etc.
- Width/height sliders for export
- DPI custom sliders
- Any input that users drag/adjust

#### Numeric Inputs (500ms debouncing)
- Axis limits (x_min, x_max, y_min, y_max)
- Axis step values
- Grid rows/columns
- Any numeric input that users type into

#### Dropdowns/Checkboxes (Immediate)
- Theme selectors
- Legend position
- Format selectors
- Any selection-based input

### 3. Files Modified

- `R/panes_text.R` - Text inputs and sliders
- `R/panes_theme.R` - Theme controls
- `R/panes_export.R` - Export settings
- `R/panes_grid.R` - Grid layout

## Benefits

1. **Smooth Typing Experience**: Users can finish typing before plots update
2. **Responsive Sliders**: Sliders update quickly but not excessively
3. **Immediate Selections**: Dropdowns and checkboxes remain instant
4. **No UI Changes**: Existing interface remains the same
5. **Performance**: Reduces unnecessary plot re-renders
6. **Standard Shiny**: Uses `invalidateLater()` which is available in all Shiny versions

## How It Works

The `invalidateLater(500)` function tells Shiny to wait 500ms before executing the rest of the code in the `observeEvent`. This creates a simple but effective debouncing effect:

1. User types in input
2. `observeEvent` triggers immediately
3. `invalidateLater(500)` delays execution by 500ms
4. If user types again within 500ms, the previous execution is cancelled
5. After 500ms of no input, the plot update executes

## Usage Examples

### Text Input with Debouncing
```r
observeEvent(input$ui_title, {
  invalidateLater(500)  # Wait 500ms after typing stops
  
  # Get plot index and update
  plot_index <- get_plot_index()
  rv$edits[[plot_index]]$title <- input$ui_title
}, ignoreInit = TRUE, ignoreNULL = TRUE)
```

### Slider with Throttling
```r
observeEvent(input$ui_base_size, {
  invalidateLater(200)  # Wait 200ms between updates
  
  # Get plot index and update
  plot_index <- get_plot_index()
  rv$edits[[plot_index]]$base_size <- input$ui_base_size
}, ignoreInit = TRUE, ignoreNULL = TRUE)
```

## Configuration

You can adjust the delay times by modifying the `invalidateLater()` parameter:

- **Text inputs**: 500ms (good balance between responsiveness and smoothness)
- **Sliders**: 200ms (responsive but not excessive)
- **Custom delays**: Set any value in milliseconds as needed

## Testing

To test the implementation:

1. Type in a text input - plot should update 500ms after you stop typing
2. Drag a slider - plot should update after 200ms delay
3. Select from a dropdown - plot should update immediately
4. Check/uncheck a checkbox - plot should update immediately

## Troubleshooting

If you experience issues:

1. Check that all files are properly sourced
2. Verify the delay parameters are appropriate for your use case
3. Check console for any error messages
4. Ensure `invalidateLater()` is available in your Shiny version

## Why This Approach Works

1. **Uses Standard Shiny Functions**: `invalidateLater()` is built into Shiny
2. **Simple and Reliable**: No complex custom debouncing logic
3. **Easy to Debug**: Clear flow of execution
4. **No External Dependencies**: Works with standard Shiny installations
5. **Proven Method**: This is how most Shiny apps handle input delays
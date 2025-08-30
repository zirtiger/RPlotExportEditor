# Debouncing Implementation for Input Interruption Prevention

## Problem Solved

This implementation addresses the common Shiny reactivity issue where typing in text inputs or adjusting sliders would be interrupted by immediate plot updates, making it difficult to complete input tasks.

## Solution Overview

We've implemented a hybrid approach using:
- **Debouncing** for text inputs and numeric inputs (500ms delay)
- **Throttling** for sliders (200ms delay) 
- **Immediate updates** for dropdowns and checkboxes

## Implementation Details

### 1. New Helper Functions (`R/helpers_debounce.R`)

```r
# Debounced observeEvent wrapper
debounced_observeEvent(eventExpr, handlerExpr, delay = 500, ...)

# Throttled observeEvent wrapper  
throttled_observeEvent(eventExpr, handlerExpr, delay = 200, ...)
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

- `R/helpers_debounce.R` - New utility functions
- `R/panes_text.R` - Text inputs and sliders
- `R/panes_theme.R` - Theme controls
- `R/panes_export.R` - Export settings
- `R/panes_grid.R` - Grid layout
- `app.R` - Added helper file to source order

## Benefits

1. **Smooth Typing Experience**: Users can finish typing before plots update
2. **Responsive Sliders**: Sliders update quickly but not excessively
3. **Immediate Selections**: Dropdowns and checkboxes remain instant
4. **No UI Changes**: Existing interface remains the same
5. **Performance**: Reduces unnecessary plot re-renders

## Usage Examples

### Before (Immediate Updates)
```r
observeEvent(input$ui_title, {
  # Plot updates immediately on every keystroke
  rv$edits[[plot_index]]$title <- input$ui_title
}, ignoreInit = TRUE, ignoreNULL = TRUE)
```

### After (Debounced Updates)
```r
debounced_observeEvent(input$ui_title, {
  # Plot updates 500ms after user stops typing
  rv$edits[[plot_index]]$title <- input$ui_title
}, delay = 500, ignoreInit = TRUE, ignoreNULL = TRUE)
```

### Slider with Throttling
```r
throttled_observeEvent(input$ui_base_size, {
  # Plot updates every 200ms while dragging
  rv$edits[[plot_index]]$base_size <- input$ui_base_size
}, delay = 200, ignoreInit = TRUE, ignoreNULL = TRUE)
```

## Configuration

You can adjust the delay times by modifying the `delay` parameter:

- **Text inputs**: 500ms (good balance between responsiveness and smoothness)
- **Sliders**: 200ms (responsive but not excessive)
- **Custom delays**: Set any value in milliseconds as needed

## Testing

To test the implementation:

1. Type in a text input - plot should update 500ms after you stop typing
2. Drag a slider - plot should update every 200ms while dragging
3. Select from a dropdown - plot should update immediately
4. Check/uncheck a checkbox - plot should update immediately

## Troubleshooting

If you experience issues:

1. Check that `R/helpers_debounce.R` is properly sourced
2. Verify the delay parameters are appropriate for your use case
3. Ensure the helper functions are available in your environment
4. Check console for any error messages

## Future Enhancements

Potential improvements could include:

- User-configurable delay times
- Different delays for different input types
- Visual feedback during debouncing/throttling
- Integration with Shiny's built-in debouncing features
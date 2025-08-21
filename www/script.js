// JavaScript for Plot Editor

// Handle menu state updates from server
Shiny.addCustomMessageHandler("updateMenuState", function(data) {
  // Update text menu item
  const textMenuItem = document.querySelector('a[data-value="text"]');
  if (textMenuItem) {
    if (data.text_active) {
      textMenuItem.classList.remove('disabled');
      textMenuItem.style.opacity = '1';
      textMenuItem.style.pointerEvents = 'auto';
    } else {
      textMenuItem.classList.add('disabled');
      textMenuItem.style.opacity = '0.5';
      textMenuItem.style.pointerEvents = 'none';
    }
  }
  
  // Update theme menu item
  const themeMenuItem = document.querySelector('a[data-value="theme"]');
  if (themeMenuItem) {
    if (data.theme_active) {
      themeMenuItem.classList.remove('disabled');
      themeMenuItem.style.opacity = '1';
      themeMenuItem.style.pointerEvents = 'auto';
    } else {
      themeMenuItem.classList.add('disabled');
      themeMenuItem.style.opacity = '0.5';
      themeMenuItem.style.pointerEvents = 'none';
    }
  }
});

// Initialize menu state on page load
$(document).ready(function() {
  // Add disabled class styling
  const style = document.createElement('style');
  style.textContent = `
    .sidebar-menu li a.disabled {
      color: #999 !important;
      cursor: not-allowed !important;
    }
    .sidebar-menu li a.disabled:hover {
      background-color: transparent !important;
    }
  `;
  document.head.appendChild(style);
});
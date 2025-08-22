// Plot Editor client helpers
Shiny.addCustomMessageHandler('activeTabChanged', function(active) {
  var sidebar = document.querySelector('.sidebar-menu');
  if (!sidebar) return;
  var gridOnly = ['grid'];
  var plotOnly = ['export', 'text', 'theme'];
  // Map menuItem text -> tabName as used in server/ui (lowercase)
  var items = sidebar.querySelectorAll('li.treeview');
  items.forEach(function(li){
    var a = li.querySelector('a');
    if (!a) return;
    var label = (a.textContent || '').trim().toLowerCase();
    var isGrid = (label === 'grid');
    var isPlot = (label === 'export' || label === 'text' || label === 'theme');
    li.classList.remove('disabled');
    if (active === 'Grid') {
      // In grid preview: disable plot-only menus (they do nothing)
      if (isPlot) li.classList.add('disabled');
    } else {
      // In plot preview: disable grid-only menu
      if (isGrid) li.classList.add('disabled');
    }
  });
  // Also prevent clicks on disabled
  sidebar.addEventListener('click', function(e){
    var li = e.target.closest('li');
    if (li && li.classList.contains('disabled')) {
      e.stopPropagation();
      e.preventDefault();
    }
  }, { capture: true });
});
// Lists ordered by dragging (`.q-order`, e.g. the groups of the exported tables, app.R): the items
// are columns side by side; dropping one on another puts it before (left half) or after (right
// half) it. The order is sent to Shiny as the input named by the list's `data-input` (an array of
// the items' `data-value`).
(function () {
  function send(box) {
    if (!box || !window.Shiny || !Shiny.setInputValue) return;
    var vals = Array.prototype.map.call(box.querySelectorAll('.q-order-item'), function (e) {
      return e.getAttribute('data-value');
    });
    Shiny.setInputValue(box.getAttribute('data-input'), vals);
  }
  function item(e) { return e.target && e.target.closest ? e.target.closest('.q-order-item') : null; }

  document.addEventListener('dragstart', function (e) {
    var it = item(e);
    if (!it) return;
    it.classList.add('q-dragging');
    e.dataTransfer.effectAllowed = 'move';
    try { e.dataTransfer.setData('text/plain', it.getAttribute('data-value')); } catch (err) {}
  });
  document.addEventListener('dragover', function (e) {
    var box = e.target && e.target.closest ? e.target.closest('.q-order') : null;
    if (!box) return;
    var drag = box.querySelector('.q-dragging');
    if (!drag) return;
    e.preventDefault();
    var over = item(e);
    if (!over || over === drag) return;
    var r = over.getBoundingClientRect();
    if (e.clientX < r.left + r.width / 2) box.insertBefore(drag, over);
    else box.insertBefore(drag, over.nextSibling);
  });
  document.addEventListener('drop', function (e) {
    if (e.target && e.target.closest && e.target.closest('.q-order')) e.preventDefault();
  });
  document.addEventListener('dragend', function (e) {
    var it = item(e);
    if (!it) return;
    it.classList.remove('q-dragging');
    send(it.parentNode);
  });
})();

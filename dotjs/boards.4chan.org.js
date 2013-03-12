if (window.location.protocol !== "https:") {
  window.location.protocol = "https:";
} else {
  // Add "Expand All" button
  $(document).ready(function() {
      var btn = document.createElement('button');
      $(btn).text('Expand All Images').on('click', function() {
          var scriptTxt = "var imgs = document.getElementsByTagName('img'); " +
            "for (var idx in imgs) { " +
            "var i = imgs[idx]; " +
            "if (i.hasAttribute('data-md5') && " +
                "!i.parentNode.parentNode.classList.contains('image-expanded')) " +
                  "ImageExpansion.expand(i); }";
          var scriptTag = document.createElement('script');
          scriptTag.textContent = scriptTxt;
          document.body.appendChild(scriptTag);
        });
      $('.navLinks').get(0).appendChild(btn);
    }
  );
}

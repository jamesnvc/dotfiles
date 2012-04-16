// jQuery is included in dotjs - can we use that to streamline this?
var bdy = document.getElementById("comic_body");
bdy.appendChild(document.createTextNode(bdy.getElementsByTagName("img")[0].title));

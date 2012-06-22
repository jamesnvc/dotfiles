var altText = document.createElement('p');
altText.appendChild(document.createTextNode($('#comic img').attr('title')));
$('#comic').after(altText);

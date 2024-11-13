:- module(web2cbz, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(sgml), [load_html/3]).
:- use_module(library(xpath)).

find_image_url(DOM, ImageLink) :-
    once(xpath(DOM, //div(@id=comic)/a/img(@src), ImageLink)).
find_image_url(DOM, ImageLink) :-
    once(xpath(DOM,
               //article/div(@class='post-content')/div(@class=entry)/p/img(@'data-orig-file'),
               ImageLink)).

find_next_url(DOM, NextLink) :-
    once(xpath(DOM, //a(@class='comic-nav-base comic-nav-next', @href), NextLink)).

process_page(Counter, URL) :-
    load_html(URL, DOM, []),
    debug(web2cbz, "Got DOM for ~w", [URL]),
    find_image_url(DOM, ImageLink),
    debug(web2cbz, "  Got image link ~w", [ImageLink]),
    format(string(ImageFileName), "images/~|~`0t~d~3+.jpg", [Counter]),
    setup_call_cleanup(open(ImageFileName, write, OutStream, [type(binary)]),
                       http_get(ImageLink, _, [to(stream(OutStream))]),
                       close(OutStream)
                      ),
    debug(web2cbz, "   Saved image link to ~w", [ImageFileName]),
    ( find_next_url(DOM, NextLink)
    -> ( Counter1 is Counter + 1,
         sleep(1),
         process_page(Counter1, NextLink))
    ; true ).

% web2cbz:process_page(0, 'https://swanboy.com/comic/swan-boy-joins-the-gym/').
% web2cbz:process_page(28, 'https://swanboy.com/comic/208/').
% done at 317, https://swanboy.com/comic/capitalonebankguy6/

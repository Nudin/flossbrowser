FFP-Project: FLOSS-Browser based on  Wikidata
=============================================

A Yesod web app display and filter Free, Libre and Open Source Software
-----------------------------------------------------------------------

### Compilation
    Change into the `src` folder and run `make`.

### Run
    First create the backend database by running `./CreateDB`.
    In case that a network error occurs, wikidata might be busy or slow.
    Simply run the program again a few seconds later.

    Afterwards run `./Browser` and visit the website on `127.0.0.1:3000`.
    The port can be changed in `flossrc`.

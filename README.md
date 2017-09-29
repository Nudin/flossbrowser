FFP-Project: FLOSS-Browser based on  Wikidata
=============================================

A Yesod web app display and filter Free, Libre and Open Source Software
-----------------------------------------------------------------------
There are two separate components: the backend wich queries Wikidata and saves
the information in a local SQL-Database and Yesod-based Web-Frontend displaying
the data from the SQL-Database. While the Frontend is intended to run as a
service, the backend is planed to be started regularly by a cronjob.

The service is already deployed at:
    https://tools.wmflabs.org/flossbrowser

### Dependencies
    The following haskell-packages are needed for compilation:
      yesod
      yesod-static
      yesod-persistent
      persistent-sqlite
      esqueleto
      http-client
      http-client-tls
      network-uri
      configurator
    You can install them with cabal or your package-manager.

### Compilation
    Change into the `src` folder and run `make`.

### Run
    First create the backend database by running `./CreateDB`.
    In case that a network error occurs, wikidata might be busy or slow.
    Simply run the program again a few seconds later.

    Afterwards run `./Browser` and visit the website on `127.0.0.1:3000`.
    The port can be changed in `flossrc`.

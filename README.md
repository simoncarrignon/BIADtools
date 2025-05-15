This gather a few tools I developped around the BIAD database.
The main one being a shiny app that hhas been move in a cleaner git here:https://github.com/BIADcore/BIADminishiny


## Shiny App:


There are multiple ways to define/run a shiny app, I went for the the run shiny app from a folder way. Which meen we need a folder with `server.R` and `ui.R`

To run the shiny app this way one need to run:

```bash
Rscript -e "shiny::shinyAppDir('shiny-app',options=list(port=1111))" #I like to use the port option, if you don't specify it shiny create a different port each time, ennoying for debbuging
```

This repo relies on BIADwiki/BIADwiki to interact with the database. For know I clone and link the dataset using symlink. When repo will be package-like form, it could be installed and update.

```bash
git clone https://github.com/BIADwiki/BIADwiki ../ #if you are in `BIADtools` folder
ln -s ../BIADwiki/R .
```


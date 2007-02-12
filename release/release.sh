#! /bin/sh -

. config.sh
. tag.sh



#build source tar (with doc) 
( cd $WORKDIR ;
  cvs -d ${CVSDIR} export ${CVSEXPORT} -l htmlgen htmlgen/html htmlgen/text htmlgen/info htmlgen/mappings htmlgen/doc htmlgen/doc/thai )

#Recompile (test)
( cd $WORKDIR/htmlgen ; make opt )
#Recompile (produce doc)
( cd $WORKDIR/htmlgen/doc ; make manual.ps manual.pdf opt )
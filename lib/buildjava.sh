#!/bin/bash -ex
# build and install the appengine java backend

OCAMLJAVA=${OCAMLJAVA:-ocamljava}
JAVAC=${JAVAC:-javac}
JAVA=${JAVA:-java}
SUDO=${SUDO:-sudo}
OCAMLFIND=${OCAMLFIND:-ocamlfind}

SOURCES_ML="appengine_datastore.ml appengine_backend.ml weakid.ml appengine_save.ml appengine_get.ml ae_db.ml"
SOURCES_MLI="appengine_datastore.mli weakid.mli ae_db.mli"
SOURCES_CMJ=$(echo ${SOURCES_ML} | sed -e 's/\.ml/.cmj/g')

CADMIUM_DIR=${CADMIUM_DIR:-~/src/oss/java/ocaml-appengine/dist}
PATH_appengine=${CADMIUM_DIR}/appengine-api-1.0-sdk-1.3.1.jar
PATH_ocamlrun=${CADMIUM_DIR}/ocamlrun.jar
PATH_ocamlwrap=${CADMIUM_DIR}/ocamlwrap.jar
PATH_ocamlrun_servlet=${CADMIUM_DIR}/ocamlrun-servlet.jar

OCAMLBCFLAGS="-classpath ${PATH_ocamlwrap}:${PATH_ocamlrun_servlet}:${PATH_appengine}:. -I +cadmium -provider fr.x9c.cadmium.primitives.cadmiumservlet.Servlets -provider org.openmirage.orm.prims.Appengine_datastore"

rm -rf org
mkdir -p org/openmirage/orm/prims
env CLASSPATH=${PATH_ocamlwrap}:${PATH_appengine} ${JAVA} fr.x9c.nickel.Main --java-dir=org/openmirage/orm/prims --java-package=org.openmirage.orm.prims appengine.nickel
${JAVAC} -target 1.6 -cp ${PATH_ocamlrun}:${PATH_appengine} org/openmirage/orm/prims/Appengine_datastore.java
${OCAMLJAVA} -i -I +cadmium appengine_datastore.ml > appengine_datastore.mli
${OCAMLJAVA} ${OCAMLBCFLAGS} -for-pack Orm -java-package org.openmirage.orm -c -annot -I +cadmium -I +site-lib/dyntype -I +site-lib/shelf ${SOURCES_MLI} ${SOURCES_ML}

${OCAMLJAVA} -pack ${SOURCES_CMJ} -o orm.cmj

jar cvf orm_ae.jar org

ODIR=`${OCAMLFIND} printconf path`/orm
${SUDO} mkdir -p ${ODIR}
${SUDO} cp orm.cmj orm.cmi orm.jo orm_ae.jar ${ODIR}/

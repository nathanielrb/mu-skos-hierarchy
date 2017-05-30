#!/bin/bash

export MU_DEFAULT_GRAPH="http://data.europa.eu/eurostat/ECOICOP"

export SPARQL_ENDPOINT="http://172.31.63.185:8890/sparql"

export CONCEPT_SCHEME="http://data.europa.eu/eurostat/id/taxonomy/ECOICOP"

export INCLUDED_PROPERTIES="name=skos:altLabel,description=skos:prefLabel,notation=skos:notation"

export DEFAULT_LANGUAGE=en

awful hierarchy.scm --port=7890

# mu-skos-hierarchy

Gets a Skos concept tree in JSON-API or JSON-LD format.

## API

### GET /schemes

Returns all Skos concept schemes in the database.

Optional parameters: `format` ("json-api" or "json-ld", defaults to "json-api")

### GET /schemes/<scheme-id>

Returns all top concepts in the scheme with mu:uuid `scheme-id`, or in  the default CONCEPT_SCHEME, if <scheme-id> is `_default`

Optional parameter: `format`

### GET /schemes/<scheme-id>/<concept-id>/descendants

Returns the descending hierarchy from the concept with mu:uuid <concept-id>, or the first top concept, if <concept-id> is `_top`

Optional parameters: `levels` (defaults to 1), `lang` (language tag for optional included properties, defaults to "en"), `format`

### GET /schemes/<scheme-id>/<concept-id>/ancestors

Returns the ascending hierarchy from the concept with mu:uuid <concept-id>, or the first top concept, if <concept-id> is `_top`

Optional parameters: `levels` (defaults to 1), `lang`, `format`


## Running

### Environment Variables

**required** MU_DEFAULT_GRAPH

**required** SPARQL_ENDPOINT

**optional** CONCEPT_SCHEME: default Skos concept scheme

**optional** INCLUDED_PROPERTIES: comma separated list of labels and predicates for propertiers to be included in results, as in:

```
description=skos:prefLabel,notation=skos:notation,name=app:name
```

**optional** DEFAULT_LANGUAGE: for language-tagged included properties

**optional** NAMESPACES: namespace definitions for use in INCLUDED_PROPERTIES

### Running in docker-compose

```
services:
  hierarchy:
    image: nathanielrb/mu-skos-hierarchy
    environment:
      MU_DEFAULT_GRAPH: "http://data.europa.eu/eurostat/ECOICOP"
      MU_SPARQL_ENDPOINT: "http://db:8890/sparql"
      CONCEPT_SCHEME: "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP"
      INCLUDED_PROPERTIES: "description=skos:prefLabel,notation=skos:notation,name=app:name"
      NAMESPACES: "mu: <http://mu.semte.ch/vocabularies/core>,app: <http://mu.semte.ch/application>"
      DEFAULT_LANGUAGE: en
    ports:
      - "4028:4028"
    links:
      - db:database
  ...
```

### Running natively

Requires Chicken Scheme 4.9+ (http://code.call-cc.org/) 

```
wget https://code.call-cc.org/releases/4.9.0/chicken-4.9.0.1.tar.gz && \
   tar xf chicken-4.9.0.1.tar.gz && \
   cd  chicken-4.9.0.1 && \
   sudo make PLATFORM=linux install && \
   cd ../ && \
   rm -r chicken-4.9.0.1  chicken-4.9.0.1.tar.gz
```

and then installing the following Eggs:

```
sudo chicken-install spiffy spiffy-request-vars srfi-69 matchable
```

and the following non-distributed Eggs:

- https://github.com/nathanielrb/s-sparql
- https://github.com/nathanielrb/mu-chicken-support

(download, and run ```sudo chicken-install``` in each directory).


## To Do

- move configuration (esp. included properties) to config file
- allow POST calls with node URI instead of mu:uuids
- improve speed
- allow included links as well as properties??
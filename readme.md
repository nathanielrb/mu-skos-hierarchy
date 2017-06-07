# mu-skos-hierarchy

Gets a Skos concept tree in JSON-API or JSON-LD format.

## API

### GET /schemes

Returns all Skos concept schemes in the database.

Optional parameter: `format` ("json-api" or "json-ld", defaults to "json-api")

### GET /schemes/:scheme-id

Returns all top concepts in the scheme with mu:uuid `scheme-id`, or in  the default CONCEPT_SCHEME, if `scheme-id` is `_default`

Optional parameter: `format`

### GET /schemes/:scheme-id/:concept-id/descendants

Returns the descending hierarchy from the concept with mu:uuid `concept-id`, or the first top concept, if `concept-id` is `_top`. (`scheme-id` can also be "_default".)

Optional parameters: `levels` (defaults to 1), `lang` (language tag for optional included properties, defaults to DEFAULT_LANG), `format`

### GET /schemes/:scheme-id/:concept-id/ancestors

Returns the ascending hierarchy from the concept with mu:uuid `concept-id`, or the first top concept, if `concept-id` is `_top`

Optional parameters: `levels` (defaults to 1), `lang`, `format`

## Example

When the default concept scheme is specified in the configuration (see below) and contains only one top concept (skos:TopConcept), then this call will return the hierarchy to 5 levels:

```
/schemes/_default/_top?levels=5
```

## Running

### Environment Variables

**required** MU_DEFAULT_GRAPH

The default graph.

**required** MU_SPARQL_ENDPOINT

The SPARQL endpoint.

**optional** CONCEPT_SCHEME

The default Skos concept scheme.

**optional** INCLUDED_PROPERTIES

A comma-separated list of properties in the form "label=predicate" to be included in results, as in:

```
description=skos:prefLabel,notation=skos:notation,name=app:name
```

Note that if INCLUDED_PROPERTIES are specified, only nodes with those properties will be returned.

**optional** DEFAULT_LANGUAGE

Default language included properties with language tags, defaults to "en".

**optional** MU_NAMESPACES

A comma-separated list of amespace definitions for use in INCLUDED_PROPERTIES, in the form ``prefix: <uri>`.

### Running in Docker

In the docker-compose file:

```
services:
  hierarchy:
    image: nathanielrb/mu-skos-hierarchy
    environment:
      MU_DEFAULT_GRAPH: "http://data.europa.eu/eurostat/ECOICOP"
      MU_SPARQL_ENDPOINT: "http://db:8890/sparql"
      CONCEPT_SCHEME: "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP"
      INCLUDED_PROPERTIES: "description=skos:prefLabel,notation=skos:notation,name=app:name"
      MU_NAMESPACES: "mu: <http://mu.semte.ch/vocabularies/core>,app: <http://mu.semte.ch/application>"
    ports:
      - "4028:4028"
    links:
      - db:database
  ...
```

or directly:

```
docker run -d \
  -p 4028:4028 \
  -e  MU_DEFAULT_GRAPH="http://data.europa.eu/eurostat/ECOICOP"\
  -e  MU_SPARQL_ENDPOINT="http://127.0.0.1:8890/sparql"\
  -e  CONCEPT_SCHEME="http://data.europa.eu/eurostat/id/taxonomy/ECOICOP"\
  -e  INCLUDED_PROPERTIES="description=skos:prefLabel,notation=skos:notation,name=app:name"\
  -e  MU_NAMESPACES="mu: <http://mu.semte.ch/vocabularies/core>,app: <http://mu.semte.ch/application>"\
  nathanielrb/mu-skos-hierarchy
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

Then the service can be run after exporting the environment variables:

```
export MU_DEFAULT_GRAPH="http://data.europa.eu/eurostat/ECOICOP"
export MU_SPARQL_ENDPOINT="http://127.0.0.1:8890/sparql"
csi app.scm
```

## To Do

- move configuration (esp. included properties) to config file
- allow POST calls with node URI instead of mu:uuids
- improve speed
- allow included links as well as properties??
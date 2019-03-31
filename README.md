# KnoDE: Knowledge Development Environment

[![Build Status](https://travis-ci.org/knocean/knode.svg?branch=master)](https://travis-ci.org/knocean/knode)

# Building the Demo Server

1. `nix-env -i leiningen git screen`
2. `git clone git@github.com:knocean/knode.git`
3. Clone your desired project _(for example, [`git@github.com:IEDB/ONTIE.git`](https://github.com/IEDB/ONTIE) or [`git@github.com:knocean/knode-example.git`](https://github.com/knocean/knode-example))_
4. Set up SSH keys using `ssh-keygen` _(necessary for `knode` to push edits to the ontology)_
5. Create a named `screen` session with `screen -S knode`
6. Load your ontology into `knode` state _(if you're setting up an ontology with a `load.txt` file, you can just do `KNODE_PROJECT_DIR=<your project directory> lein run load`. Otherwise, you'll need to pass in the project name and files in the proper order. For instance `lein run load ONTIE ~/ONTIE/ontology/context.kn ~/ONTIE/ontology/external.tsv ~/ONTIE/ontology/predicates.tsv ~/ONTIE/ontology/index.tsv ~/ONTIE/ontology/templates.kn ~/ONTIE/ontology/ontie.kn`)_
7. `KNODE_SSH_PASSPHRASE=<your key passphrase> KNODE_PROJECT_DIR=<your project dir> lein run serve` _(the first time you run this, it will take a while because new `JAR`s)_

## Configuration Files

You may provide a set of configuration options via an EDN file. By default, this can be `knode.edn` in the same directory that you run `knode` in. In this file, you may specify a number of configurators:

* `:project-name`: name of the project
* `:project-dir`: directory to house resources and `knode.db` file
* `:base-iri`: base IRI for ontology (default `http://ontology.iedb.org/ontology/ONTIE_`)
* `:port`: port to run the server on (default `3210`)
* `:database-url`: URL to database to store resources and states (default `jdbc:sqlite:<absolute path>/knode.db`)
* `:git-repo`: path to git repository
* `:ssh-identity`: SSH identity for git (default `~/.ssh/id_rsa`)
* `:ssh-passphrase`: SSH passphrase for git
* `:api-key`
* `:write-file`
* `:resources`: see below

To run KnODE from a configuration file (replace `knode.edn` if necessary):
```
knode serve knode.edn
```

To view the configuration settings:
```
knode config knode.edn
```

#### Resources

To load resources from the configuration file (if you are using `knode.edn`, you do not need to specify the path):
```
knode load-config
```

Or:
```
knode load-config <path to edn>
```

Resources should be provided as a vector of individual resources. Each resource needs, minimally:

* `:idspace`
* `:type`

You may also include:

* `:label` and/or `:title`
* `:description`
* `:homepage`

Valid types of resources are:

* `:local-file`: include a `:path` or `:paths` to local resources
* `:obo-published-owl`: use the `:idspace` to download an OBO published resource (must be the same as the OBO ID)
* `:direct-link`: include a `:url` to point to the remote resource
* `:obo-github-repo`: include a `:repo` pointing to a `.git` repository in the standard OBO directory layout

#### Example Config

```
{:project-name "ONTIE",
 :resources
 [{:idspace "ONTIE",
   :title "Ontology for Immune Epitopes",
   :description
   "ONTIE includes a range of terms to support the Immune Epitope Database.",
   :homepage "https://ontology.iedb.org",
   :type :local-file,
   :paths
   ["ontology/context.kn"
    "ontology/external.tsv"
    "ontology/predicates.tsv"
    "ontology/index.tsv"
    "ontology/templates.kn"
    "ontology/ontie.kn"]}
  {:idspace "OBI",
   :title "Ontology for Biomedical Investigations",
   :homepage "http://obi-ontology.org",
   :type :obo-published-owl}]}
```

## License

Copyright © 2017 Knocean Inc.

Distributed under the BSD 3-Clause License.

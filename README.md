# KnoDE: Knowledge Development Environment

[![Build Status](https://travis-ci.org/knocean/knode.svg?branch=master)](https://travis-ci.org/knocean/knode)

# Building the Demo Server

1. `nix-env -i leiningen git screen`
2. `git clone git@github.com:knocean/knode.git`
3. Clone your desired project _(for example, [`git@github.com:IEDB/ONTIE.git`](https://github.com/IEDB/ONTIE) or [`git@github.com:knocean/knode-example.git`](https://github.com/knocean/knode-example))_
4. Set up SSH keys using `ssh-keygen` _(necessary for `knode` to push edits to the ontology)_
5. Create a named `screen` session with `screen -S knode`
6. Load your ontology into `knode` state _(if you're setting up ONTIE, you can do that with `lein run load ONTIE ~/ONTIE/ontology/context.kn ~/ONTIE/ontology/external.tsv ~/ONTIE/ontology/predicates.tsv ~/ONTIE/ontology/index.tsv ~/ONTIE/ontology/templates.kn ~/ONTIE/ontology/ontie.kn`)_
7. `SSH_PASSPHRASE=<your key passphrase> PROJECT_DIR=<your project dir> lein run serve` _(the first time you run this, it will take a while because new `JAR`s)_

## License

Copyright © 2017 Knocean Inc.

Distributed under the BSD 3-Clause License.

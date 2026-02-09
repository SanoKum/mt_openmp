#!/bin/bash
# Show unified diff between stage.log and stage.log.make_omp2
cd "$(dirname "$0")"
diff -u stage.log.make_omp2 stage.log | less
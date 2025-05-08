## Useful commands

zip stack_project.zip -r cql/cql22-0.0.5.vsix app/ src/ test/ package.yaml plc.cabal Setup.hs stack.yaml stack.yaml.lock CHANGELOG.md README.md -x src/*.cql
mkdir ~/haskql
mv stack_project.zip ~/haskql
unzip ~/haskql/stack_project.zip
rm ~/haskql/stack_project.zip
zip ~/stack_project.zip -r ~/haskql

zip tasks.zip -r t1.cql t2.cql t3.cql t4.cql t5.cql

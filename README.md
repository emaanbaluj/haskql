## Useful commands

zip stack_project.zip -r cql/cql22-0.0.5.vsix app/ src/ test/ package.yaml plc.cabal Setup.hs stack.yaml stack.yaml.lock A.csv B.csv P.csv Q.csv CHANGELOG.md README.md -x src/*.cql
mkdir ~/haskql
mv stack_project.zip ~/haskql
cd ~/haskql
unzip stack_project.zip
cd ..
rm haskql/stack_project.zip
zip -r haskql/ stack_project.zip

zip tasks.zip -r src/t1.cql src/t2.cql src/t3.cql src/t4.cql src/t5.cql

# Trabalho Final da disciplina de Semântica Formal

Implementação da linguagem L1 feita na linguagem OCaml.

O programa esta dividido em módulos. Para carregar todos os arquivos do projeto basta rodar no interpretador OCaml:
```
#use "useAll.ml";;
```
A sintaxe abstrata está definida no arquivo "syntax.ml".

As informações notacionais sobre a semântica operacional estão no pdf entitulado de "big_step.pdf".


O avaliador big-step para linguagem L1 extendida com listas e exceções se encontra no arquivo "evaluator.ml".

Foram realizados diversos testes com o intuito de cobrir todas as regras da semântica operacional big-step para L1. Todos eles se encontram no arquivo "tests.ml". Para rodá-los basta utilizar o comando no interpretador OCaml:
```
#use "tests.ml";;
```
Dessa forma, será possível visualizar todos os test-cases e os resultados obtidos a partir de sua execução.

O inferidor de tipos encontra-se no arquivo "typeInfer.ml".

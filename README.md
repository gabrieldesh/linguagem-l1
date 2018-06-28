# Trabalho Final da disciplina de Semântica Formal
Integrantes do grupo:
Arthur Zachow;
Gabriel Haggstrom;
Gabriel Pakulski.

Implementação da Linguagem L1 feita na linguagem OCaml.

O programa esta dividido em módulos;

A sintaxe abstrata está definida no arquivo "syntax.ml";

As informações notacionais sobre a Semântica Operacional estão no pdf entitulado de "big_step.pdf";

O Avaliador Big-Step para Linguagem L1 extendida com listas e exceções se encontra no arquivo "evaluator.ml". Foram realizados diversos testes com o intuito de cobrir todas as regras da Semântica Operacional Big-Step para L1, todos eles se encontram no arquivo "tests.ml", para rodá-los basta utilizar o comando no prompt #use "useAll.ml";; seguido de #use "tests.ml";;. Dessa forma, será possível visualizar todos os test-cases e os resultados obtidos a partir de sua execução.
 

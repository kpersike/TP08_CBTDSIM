      ******************************************************************
      * IFSP - CAMPUS CUBATAO                                         *
      * DISCIPLINA: DSIM - COBOL                                      *
      *                                                                *
      * INTEGRANTES:                                                   *
      * Adriano Junior de Souza Almeida - CB3030644                    *
      * Kaik Persike Maiorquino - CB3029689                            *
      *                                                                *
      * TITULO: TRABALHO PRATICO 08 - MANIPULACAO DE ARQUIVOS          *
      *         SEQUENCIAIS                                           *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP08.
       AUTHOR "Adriano Junior de Souza Almeida (CB3030644)"
              "Kaik Persike Maiorquino (CB3029689)".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-ALUNOS
               ASSIGN TO "ALUNOS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD ARQ-ALUNOS.
       01 REG-ALUNO.
          05 IN-MATRICULA              PIC X(6).
          05 FILLER                    PIC X.
          05 IN-NOME                   PIC X(40).
          05 FILLER                    PIC X.
          05 IN-NOTA                   PIC X(3).
          05 FILLER                    PIC X.
          05 IN-FREQUENCIA             PIC X(3).

       WORKING-STORAGE SECTION.

       01 TABELA-ALUNOS.
          05 TAB-ALUNO OCCURS 5000 TIMES.
             10 TAB-MATRICULA          PIC X(6).
             10 TAB-SEPARADOR-1        PIC X.
             10 TAB-NOME               PIC X(40).
             10 TAB-SEPARADOR-2        PIC X.
             10 TAB-NOTA               PIC X(3).
             10 TAB-SEPARADOR-3        PIC X.
             10 TAB-FREQUENCIA         PIC X(3).

       01 WS-STATUS                    PIC XX.
       01 WS-OPCAO                     PIC X.
       01 WS-FIM                       PIC X VALUE 'N'.
       01 WS-SAIR                      PIC X VALUE 'N'.
       01 WS-ACHOU                     PIC X.
       01 WS-LIMITE                    PIC X VALUE 'N'.
       01 WS-LEITURA-OK                PIC X VALUE 'S'.
       01 WS-GRAVACAO-OK               PIC X.
       01 WS-TOTAL                     PIC 9(5) COMP-5 VALUE 0.
       01 WS-I                         PIC 9(5) COMP-5.
       01 WS-J                         PIC 9(5) COMP-5.
       01 WS-POSICAO                   PIC 9(5) COMP-5.
       01 WS-AUXILIAR                  PIC X(55).
       01 WS-MATRICULA                 PIC X(6).
       01 WS-NOME                      PIC X(40).
       01 WS-NOTA                      PIC X(3).
       01 WS-FREQUENCIA                PIC X(3).
       01 WS-MENSAGEM                  PIC X(45).
       01 WS-PAUSA                     PIC X.
       01 WS-LINHA                     PIC 99 COMP-5.
       01 WS-LINHA-TEXTO               PIC X(58).

       SCREEN SECTION.

       01 TELA-MENU.
          05 LINE 02 COLUMN 02 VALUE 'A - INCLUIR REGISTRO'.
          05 LINE 03 COLUMN 02 VALUE 'B - EXCLUIR REGISTRO'.
          05 LINE 04 COLUMN 02 VALUE 'C - LISTAR REGISTROS'.
          05 LINE 05 COLUMN 02 VALUE 'D - SAIR'.
          05 LINE 07 COLUMN 02 VALUE 'OPCAO:'.

       01 TELA-INCLUSAO.
          05 LINE 02 COLUMN 02 VALUE 'MATRICULA:'.
          05 LINE 03 COLUMN 02 VALUE 'NOME:'.
          05 LINE 04 COLUMN 02 VALUE 'NOTA:'.
          05 LINE 05 COLUMN 02 VALUE 'FREQUENCIA:'.

       01 TELA-EXCLUSAO.
          05 LINE 02 COLUMN 02 VALUE 'MATRICULA:'.

       01 TELA-LISTAGEM.
          05 LINE 02 COLUMN 02 VALUE 'MATRIC'.
          05 LINE 02 COLUMN 10 VALUE 'NOME'.
          05 LINE 02 COLUMN 52 VALUE 'NOTA'.
          05 LINE 02 COLUMN 57 VALUE 'FREQ'.
          05 LINE 21 COLUMN 02 VALUE 'ENTER PARA CONTINUAR'.

       01 TELA-AVISO.
          05 LINE 03 COLUMN 02 PIC X(45)
             FROM WS-MENSAGEM.
          05 LINE 05 COLUMN 02 VALUE 'ENTER PARA CONTINUAR'.

       PROCEDURE DIVISION.

       INICIO.
           PERFORM CARREGAR-ALUNOS
           PERFORM UNTIL WS-SAIR = 'S'
               PERFORM LIMPAR-TELA
               DISPLAY TELA-MENU
               MOVE SPACE TO WS-OPCAO
               ACCEPT WS-OPCAO AT LINE 07 COLUMN 10
               EVALUATE WS-OPCAO
                   WHEN 'A'
                   WHEN 'a'
                       PERFORM INCLUIR-REGISTRO
                   WHEN 'B'
                   WHEN 'b'
                       PERFORM EXCLUIR-REGISTRO
                   WHEN 'C'
                   WHEN 'c'
                       PERFORM LISTAR-REGISTROS
                   WHEN 'D'
                   WHEN 'd'
                       MOVE 'S' TO WS-SAIR
                   WHEN OTHER
                       MOVE 'OPCAO INVALIDA' TO WS-MENSAGEM
                       PERFORM MOSTRAR-AVISO
               END-EVALUATE
           END-PERFORM
           PERFORM GRAVAR-ALUNOS
           STOP RUN.

       CARREGAR-ALUNOS.
           OPEN INPUT ARQ-ALUNOS
           IF WS-STATUS NOT = '00'
               MOVE 'ARQUIVO ALUNOS.DAT NAO ENCONTRADO'
                 TO WS-MENSAGEM
               PERFORM MOSTRAR-AVISO
               STOP RUN
           END-IF
           PERFORM UNTIL WS-FIM = 'S'
               READ ARQ-ALUNOS
                   AT END
                       MOVE 'S' TO WS-FIM
                   NOT AT END
                       IF WS-STATUS = '00'
                           IF WS-TOTAL < 5000
                               ADD 1 TO WS-TOTAL
                               MOVE REG-ALUNO
                                 TO TAB-ALUNO(WS-TOTAL)
                           ELSE
                               MOVE 'S' TO WS-FIM WS-LIMITE
                               MOVE 'LIMITE DE REGISTROS ATINGIDO'
                                 TO WS-MENSAGEM
                           END-IF
                       ELSE
                           MOVE 'S' TO WS-FIM
                           MOVE 'N' TO WS-LEITURA-OK
                           MOVE 'ERRO AO LER ALUNOS.DAT'
                             TO WS-MENSAGEM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ARQ-ALUNOS
           IF WS-LIMITE = 'S' OR WS-LEITURA-OK = 'N'
               PERFORM MOSTRAR-AVISO
               STOP RUN
           END-IF.

       INCLUIR-REGISTRO.
           IF WS-TOTAL >= 5000
               MOVE 'LIMITE DE REGISTROS ATINGIDO'
                 TO WS-MENSAGEM
               PERFORM MOSTRAR-AVISO
           ELSE
               MOVE SPACES TO WS-MATRICULA WS-NOME
                              WS-NOTA WS-FREQUENCIA
               PERFORM LIMPAR-TELA
               DISPLAY TELA-INCLUSAO
               ACCEPT WS-MATRICULA AT LINE 02 COLUMN 20
               ACCEPT WS-NOME AT LINE 03 COLUMN 20
               ACCEPT WS-NOTA AT LINE 04 COLUMN 20
               ACCEPT WS-FREQUENCIA AT LINE 05 COLUMN 20
               EVALUATE TRUE
                   WHEN WS-MATRICULA IS NOT NUMERIC
                       MOVE 'MATRICULA INVALIDA'
                         TO WS-MENSAGEM
                   WHEN WS-NOME = SPACES
                       MOVE 'NOME INVALIDO'
                         TO WS-MENSAGEM
                   WHEN WS-NOTA IS NOT NUMERIC
                       MOVE 'NOTA INVALIDA'
                         TO WS-MENSAGEM
                   WHEN WS-FREQUENCIA IS NOT NUMERIC
                       MOVE 'FREQUENCIA INVALIDA'
                         TO WS-MENSAGEM
                   WHEN OTHER
                       PERFORM BUSCAR-MATRICULA
                       IF WS-ACHOU = 'S'
                           MOVE 'MATRICULA JA CADASTRADA'
                             TO WS-MENSAGEM
                       ELSE
                           ADD 1 TO WS-TOTAL
                           MOVE SPACES TO TAB-ALUNO(WS-TOTAL)
                           MOVE WS-MATRICULA
                             TO TAB-MATRICULA(WS-TOTAL)
                           MOVE WS-NOME TO TAB-NOME(WS-TOTAL)
                           MOVE WS-NOTA TO TAB-NOTA(WS-TOTAL)
                           MOVE WS-FREQUENCIA
                             TO TAB-FREQUENCIA(WS-TOTAL)
                           MOVE 'REGISTRO INCLUIDO'
                             TO WS-MENSAGEM
                       END-IF
               END-EVALUATE
               PERFORM MOSTRAR-AVISO
           END-IF.

       EXCLUIR-REGISTRO.
           MOVE SPACES TO WS-MATRICULA
           PERFORM LIMPAR-TELA
           DISPLAY TELA-EXCLUSAO
           ACCEPT WS-MATRICULA AT LINE 02 COLUMN 20
           PERFORM BUSCAR-MATRICULA
           IF WS-ACHOU = 'S'
               MOVE WS-POSICAO TO WS-I
               PERFORM UNTIL WS-I >= WS-TOTAL
                   COMPUTE WS-J = WS-I + 1
                   MOVE TAB-ALUNO(WS-J) TO TAB-ALUNO(WS-I)
                   ADD 1 TO WS-I
               END-PERFORM
               SUBTRACT 1 FROM WS-TOTAL
               MOVE 'REGISTRO EXCLUIDO' TO WS-MENSAGEM
           ELSE
               MOVE 'MATRICULA NAO ENCONTRADA'
                 TO WS-MENSAGEM
           END-IF
           PERFORM MOSTRAR-AVISO.

       LISTAR-REGISTROS.
           IF WS-TOTAL = 0
               MOVE 'NENHUM REGISTRO CADASTRADO'
                 TO WS-MENSAGEM
               PERFORM MOSTRAR-AVISO
           ELSE
               PERFORM ORDENAR-ALUNOS
               MOVE 1 TO WS-I
               PERFORM UNTIL WS-I > WS-TOTAL
                   PERFORM LIMPAR-TELA
                   DISPLAY TELA-LISTAGEM
                   MOVE 4 TO WS-LINHA
                   PERFORM 15 TIMES
                       IF WS-I <= WS-TOTAL
                           MOVE SPACES TO WS-LINHA-TEXTO
                           STRING
                               TAB-MATRICULA(WS-I) '  '
                               TAB-NOME(WS-I) '  '
                               TAB-NOTA(WS-I) '  '
                               TAB-FREQUENCIA(WS-I)
                               DELIMITED BY SIZE
                               INTO WS-LINHA-TEXTO
                           END-STRING
                           DISPLAY WS-LINHA-TEXTO
                               AT LINE WS-LINHA COLUMN 02
                           ADD 1 TO WS-I
                           ADD 1 TO WS-LINHA
                       END-IF
                   END-PERFORM
                   MOVE SPACE TO WS-PAUSA
                   ACCEPT WS-PAUSA AT LINE 22 COLUMN 02
               END-PERFORM
           END-IF.

       BUSCAR-MATRICULA.
           MOVE 'N' TO WS-ACHOU
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TOTAL OR WS-ACHOU = 'S'
               IF TAB-MATRICULA(WS-I) = WS-MATRICULA
                   MOVE 'S' TO WS-ACHOU
                   MOVE WS-I TO WS-POSICAO
               END-IF
           END-PERFORM.

       ORDENAR-ALUNOS.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I >= WS-TOTAL
               COMPUTE WS-J = WS-I + 1
               PERFORM UNTIL WS-J > WS-TOTAL
                   IF TAB-NOME(WS-I) > TAB-NOME(WS-J)
                       MOVE TAB-ALUNO(WS-I) TO WS-AUXILIAR
                       MOVE TAB-ALUNO(WS-J) TO TAB-ALUNO(WS-I)
                       MOVE WS-AUXILIAR TO TAB-ALUNO(WS-J)
                   END-IF
                   ADD 1 TO WS-J
               END-PERFORM
           END-PERFORM.

       GRAVAR-ALUNOS.
           PERFORM ORDENAR-ALUNOS
           OPEN OUTPUT ARQ-ALUNOS
           IF WS-STATUS NOT = '00'
               MOVE 'ERRO AO ABRIR ALUNOS.DAT'
                 TO WS-MENSAGEM
           ELSE
               MOVE 'S' TO WS-GRAVACAO-OK
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TOTAL
                   OR WS-GRAVACAO-OK = 'N'
                   MOVE TAB-ALUNO(WS-I) TO REG-ALUNO
                   WRITE REG-ALUNO
                   IF WS-STATUS NOT = '00'
                       MOVE 'N' TO WS-GRAVACAO-OK
                   END-IF
               END-PERFORM
               CLOSE ARQ-ALUNOS
               IF WS-STATUS NOT = '00'
                   MOVE 'N' TO WS-GRAVACAO-OK
               END-IF
               IF WS-GRAVACAO-OK = 'S'
                   MOVE 'ALUNOS.DAT ATUALIZADO'
                     TO WS-MENSAGEM
               ELSE
                   MOVE 'ERRO AO GRAVAR ALUNOS.DAT'
                     TO WS-MENSAGEM
               END-IF
           END-IF
           PERFORM MOSTRAR-AVISO.

       LIMPAR-TELA.
           DISPLAY SPACE AT LINE 01 COLUMN 01
               WITH BLANK SCREEN.

       MOSTRAR-AVISO.
           PERFORM LIMPAR-TELA
           DISPLAY TELA-AVISO
           MOVE SPACE TO WS-PAUSA
           ACCEPT WS-PAUSA AT LINE 06 COLUMN 02.

       END PROGRAM TP08.

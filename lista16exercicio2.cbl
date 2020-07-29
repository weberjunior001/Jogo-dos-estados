      $set sourceformat"free"

      *>Divis�o de identifica��o do programa
       identification division.
       program-id. "lista16exercicio2".
       author. "Anderson Weber Junior".
       installation. "PC".
       date-written. 29/07/2020.
       date-compiled. 29/07/2020.



      *>Divis�o para configura��o do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declara��o dos recursos externos
       input-output section.
       file-control.

      *>   Declara��o do arquivo
           select arqEstados assign to "arqEstadosRel.dat"
           organization is relative
           access mode is dynamic
           lock mode is automatic
           relative key is ws-rk-arqEstados
           file status is ws-fs-arqEstados.




       i-o-control.

      *>Declara��o de vari�veis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqEstados.
       01  fd-estados.
           05 fd-estado                            pic x(25).
           05 fd-capital                           pic x(25).


      *>----Variaveis de trabalho
       working-storage section.

       77  ws-rk-arqEstados                        pic 9(02).
       77  ws-fs-arqEstados                        pic 9(02).

       01  ws-estados occurs 27.
           05 ws-estado                            pic x(25).
           05 ws-capital                           pic x(25).

       01 ws-jogadores occurs 4.
          05 ws-nome-jog                           pic x(25).
          05 ws-pontos                             pic 9(02) value zero.

       01 ws-jogadores-aux.
          05 ws-nome-jog-aux                       pic x(25).
          05 ws-pontos-aux                         pic 9(02) value zero.

       01 ws-indices.
          05 ws-ind-est                            pic 9(02).
          05 ws-ind-jog                            pic 9(01).

       01 ws-tela-menu.
          05 ws-cadastro-jogadores                 pic x(01).
          05 ws-jogar                              pic x(01).

       01 ws-tela-jogo.
          05 ws-capital-jog                        pic x(25).
          05 ws-estado-sorteado                    pic x(25).
          05 ws-pontos-jogador                     pic 9(02).

       01 ws-uso-comum.
          05 ws-sair                               pic x(01).
          05 ws-msn                                pic x(50).
          05 ws-msn-erro.
             10 ws-msn-erro-ofsset                 pic 9(04).
             10 filler                             pic x(01) value "-".
             10 ws-msn-erro-cod                    pic 9(02).
             10 filler                             pic x(01) value space.
             10 ws-msn-erro-text                   pic x(42).

          05 ws-nome-jogador                       pic x(25).

       01 sorteio.
          05  semente                              pic  9(08).
          05  num_random                           pic  9(01)V9999999.

       01 controle                                 pic x(1).
          88  trocou                               value "1".
          88  nao_trocou                           value "5".


      *>----Variaveis para comunica��o entre programas
       linkage section.


      *>----Declara��o de tela
       screen section.
       01  sc-tela-menu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Tela Principal                                   ".
           05 line 03 col 01 value "      MENU                                                                       ".
           05 line 04 col 01 value "        [ ]Cadastro de Jogadores                                                 ".
           05 line 05 col 01 value "        [ ]Jogar                                                                 ".


           05 sc-sair-menu            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cadastro-jogadores   line 04  col 10 pic x(01)
           using ws-cadastro-jogadores foreground-color 15.

           05 sc-jogar                line 05  col 10 pic x(01)
           using ws-jogar foreground-color 15.

       01  sc-tela-cad-jogador.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Jogadores                            ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "      Jogador  :                                                                 ".
           05 line 22 col 01 value "               [__________________________________________________]              ".


           05 sc-sair-cad-jog            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome-jog-cad-jog        line 04  col 17 pic x(25)
           using ws-nome-jogador foreground-color 12.

           05 sc-msn-cad-jog             line 22  col 17 pic x(50)
           from ws-msn  foreground-color 12.

       01  sc-tela-jogar.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                           Quiz Estados Brasileiros                              ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "      Jogador  :                                   Pontos Acumulados:            ".
           05 line 06 col 01 value "      Qual e a capital do estado:                                                ".
           05 line 07 col 01 value "      Resposta :                                                                 ".
           05 line 22 col 01 value "               [__________________________________________________]              ".


           05 sc-sair-jog                line 01  col 71 pic x(01)
           using ws-sair                 foreground-color 12.

           05 sc-nome-jog                line 04  col 17 pic x(25)
           from ws-nome-jogador          foreground-color 12.

           05 sc-pontos-jog              line 04  col 71 pic 9(02)
           from ws-pontos-jogador        foreground-color 12.

           05 sc-estado-sorteado-jog     line 06  col 34 pic x(25)
           from ws-estado-sorteado       foreground-color 12.

           05 sc-resposta-jog            line 07  col 17 pic x(25)
           using ws-capital-jog          foreground-color 12.

           05 sc-msn-jog                 line 22  col 17 pic x(50)
           from ws-msn                   foreground-color 12.


       01  sc-tela-relatorio.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Resultados finais                                ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 07 col 01 value "  Vencedor         :                                        Pontos:              ".
           05 line 06 col 01 value "  Segundo colocado :                                        Pontos:              ".
           05 line 05 col 01 value "  Terceiro colocado:                                        Pontos:              ".
           05 line 04 col 01 value "  Quarto colocado  :                                        Pontos:              ".
           05 line 22 col 01 value "               [__________________________________________________]              ".


           05 sc-sair-rel                line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome-jog1-rel           line 04  col 21 pic x(25)
           from ws-nome-jog(1) foreground-color 12.

           05 sc-pontos-jog1-rel         line 04  col 68 pic 9(02)
           from ws-pontos(1) foreground-color 12.

           05 sc-nome-jog2-rel           line 05  col 21 pic x(25)
           from ws-nome-jog(2) foreground-color 12.

           05 sc-pontos-jog2-rel         line 05  col 68 pic 9(02)
           from ws-pontos(2) foreground-color 12.

           05 sc-nome-jog3-rel           line 06  col 21 pic x(25)
           from ws-nome-jog(3) foreground-color 12.

           05 sc-pontos-jog3-rel         line 06  col 68 pic 9(02)
           from ws-pontos(3) foreground-color 12.

           05 sc-nome-jog4-rel           line 07  col 21 pic x(25)
           from ws-nome-jog(4) foreground-color 12.

           05 sc-pontos-jog4-rel         line 07  col 68 pic 9(02)
           from ws-pontos(4) foreground-color 12.

           05 sc-msn-rel                 line 22  col 17 pic x(50)
           from ws-msn  foreground-color 12.

      *>Declara��o do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicializa��o
      *>------------------------------------------------------------------------
       inicializa section.

      *>    inicializa��o da tabela de estados
           open input arqEstados.
           if ws-fs-arqEstados <> 0 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqEstados                 to ws-msn-erro-cod
               move "Erro ao abrir arq. arqEstados " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           move  1              to   ws-rk-arqEstados
           start arqEstados key is = ws-rk-arqEstados

           perform varying ws-ind-est from 1 by 1 until ws-fs-arqEstados = 10
                                                     or ws-ind-est > 27

               read arqEstados into  ws-estados(ws-ind-est)
               if  ws-fs-arqEstados <> 0
               and ws-fs-arqEstados <> 10 then
                   move 2                                to ws-msn-erro-ofsset
                   move ws-fs-arqEstados                 to ws-msn-erro-cod
                   move "Erro ao ler arq. arqEstados "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           end-perform

           close arqEstados.
           if ws-fs-arqEstados <> 0 then
               move 3                                 to ws-msn-erro-ofsset
               move ws-fs-arqEstados                  to ws-msn-erro-cod
               move "Erro ao fechar arq. arqEstados " to ws-msn-erro-text
               perform finaliza-anormal
           end-if
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

      *>    menu do sistema
           perform until ws-sair = "X"
                      or ws-sair = "x"

               move space  to ws-cadastro-jogadores
               move space  to ws-jogar
               move space  to ws-sair

               display sc-tela-menu
               accept sc-tela-menu

               if  ws-cadastro-jogadores  = "X"
               or  ws-cadastro-jogadores  = "x"  then
                    perform cadastrar-jogadores
               end-if

               if  ws-jogar = "X"
               or  ws-jogar = "x" then
                    perform jogar
               end-if

           end-perform

      *>   chamar impressao de relatorio
           perform relatorio-final

           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Cadastro de jogadores, sao admitidos at� 4 jogadores
      *>------------------------------------------------------------------------
       cadastrar-jogadores section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

               move space  to ws-nome-jogador

               display sc-tela-cad-jogador
               accept sc-tela-cad-jogador

               move space     to   ws-msn

               if ws-nome-jogador <> space then
                   perform descobrir-prox-ind-jog

                   if ws-ind-jog <= 4 then
                       move ws-nome-jogador   to  ws-nome-jog(ws-ind-jog)
                   else
                       move "Quantidade de jogadores completa" to ws-msn
                   end-if
               end-if

           end-perform
           .
       cadastrar-jogadores-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Motor do jogo
      *>------------------------------------------------------------------------
       jogar section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

               perform varying  ws-ind-jog  from 1 by 1 until ws-ind-jog > 4
                                                          or  ws-nome-jog(ws-ind-jog) = spaces
                                                          or  ws-sair = "V"
                                                          or  ws-sair = "v"

                   move ws-nome-jog(ws-ind-jog)   to   ws-nome-jogador
                   move ws-pontos(ws-ind-jog)     to   ws-pontos-jogador

                   perform sorteia-estado

                   move ws-estado(ws-ind-est)     to   ws-estado-sorteado

                   move space                     to   ws-capital-jog
                   move space                     to   ws-msn

                   display sc-tela-jogar
                   accept sc-tela-jogar

                   if ws-capital-jog = ws-capital(ws-ind-est) then
                         add 1 to ws-pontos(ws-ind-jog)
                         move "Acertou!!!"  to ws-msn
                   else
                         move "Errou!!!"    to ws-msn
                   end-if

                   display sc-tela-jogar
                   accept sc-tela-jogar

               end-perform

           end-perform

           .
       jogar-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Descobrir a proxima posi��o livre dentro da tabela de jogadores
      *>------------------------------------------------------------------------
       descobrir-prox-ind-jog section.
           perform varying ws-ind-jog from 1 by 1 until ws-ind-jog > 4
                                                     or ws-nome-jog(ws-ind-jog) = space
           end-perform
           .
       descobrir-prox-ind-jog-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Sorteia o estado
      *>------------------------------------------------------------------------
       sorteia-estado section.

            move zero   to   ws-ind-est

            perform until ws-ind-est <> 0

               accept semente from time
               compute num_random = function random(semente)
               multiply num_random by 27 giving ws-ind-est

            end-perform
           .
       sorteia-estado-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Imprimindo relat�rio final
      *>------------------------------------------------------------------------
       relatorio-final section.

           perform ordenar-jogadores

           move space to ws-msn
           move space to ws-sair

           display sc-tela-relatorio
           accept sc-tela-relatorio

           .
       relatorio-final-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Ordena��o da tabela de jogadores
      *>------------------------------------------------------------------------
       ordenar-jogadores section.
           set trocou  to true

           perform until nao_trocou

               move 1 to ws-ind-jog
               set nao_trocou   to true

               perform until ws-ind-jog = 4
                       or    ws-nome-jog(ws-ind-jog + 1) = space

                   if ws-pontos(ws-ind-jog) < ws-pontos(ws-ind-jog + 1) then

                       move ws-jogadores(ws-ind-jog + 1)  to  ws-jogadores-aux
                       move ws-jogadores(ws-ind-jog)      to  ws-jogadores(ws-ind-jog + 1)
                       move ws-jogadores-aux              to  ws-jogadores(ws-ind-jog)

                       set trocou         to  true
                   end-if

                   add  1   to ws-ind-jog

               end-perform

           end-perform

           .
       ordenar-jogadores-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finaliza��o  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finaliza��o Normal
      *>------------------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.


Fonte de dados:

�Raw data from online personality tests� http://personality-testing.info/_rawdata/

Respostas ao teste de personalidade Big Five, elaborado com itens do projeto �International Personality Item Pool�.

Descri��o:
50�afirma��es psicom�tricas com base na escala Likert, sexo, idade, ra�a, l�ngua materna e pa�s
n = 19719

Link para download da pasta compactada com CSV: http://personality-testing.info/_rawdata/BIG5.zip

Renomeei o arquivo para "big5.csv� 
Talvez seja necess�rio alterar a extens�o para .txt, para que os programas leiam os dados como colunas separadas (precisei fazer isso no Excel, mas n�o no Planilhas Google).

Limpeza de dados:
- 1 caso continha 0 (ausente) em todas as vari�veis de personalidade e, por isso, foi exclu�do. (1 de 19.719)
- Casos com idade acima de 80�anos foram exclu�dos (a maioria parecia estar com c�digos de dados ausentes ou anos que n�o poderiam ser facilmente codificados). (87 de 19.719)
- Casos com c�digo de pa�s inv�lido foram exclu�dos. (377 de 19.719)
- Todos os casos restantes com valores ausentes nas vari�veis demogr�ficas foram exclu�dos
- n final dos casos completos: 18.931 de 19.719 = 96%

Dados simplificados:
- b5.csv cont�m apenas as 50�vari�veis do Big�5
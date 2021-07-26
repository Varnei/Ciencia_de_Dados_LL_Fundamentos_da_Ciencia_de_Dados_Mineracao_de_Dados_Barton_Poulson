Fonte de dados:

“Raw data from online personality tests” http://personality-testing.info/_rawdata/

Respostas ao teste de personalidade Big Five, elaborado com itens do projeto “International Personality Item Pool”.

Descrição:
50 afirmações psicométricas com base na escala Likert, sexo, idade, raça, língua materna e país
n = 19719

Link para download da pasta compactada com CSV: http://personality-testing.info/_rawdata/BIG5.zip

Renomeei o arquivo para "big5.csv” 
Talvez seja necessário alterar a extensão para .txt, para que os programas leiam os dados como colunas separadas (precisei fazer isso no Excel, mas não no Planilhas Google).

Limpeza de dados:
- 1 caso continha 0 (ausente) em todas as variáveis de personalidade e, por isso, foi excluído. (1 de 19.719)
- Casos com idade acima de 80 anos foram excluídos (a maioria parecia estar com códigos de dados ausentes ou anos que não poderiam ser facilmente codificados). (87 de 19.719)
- Casos com código de país inválido foram excluídos. (377 de 19.719)
- Todos os casos restantes com valores ausentes nas variáveis demográficas foram excluídos
- n final dos casos completos: 18.931 de 19.719 = 96%

Dados simplificados:
- b5.csv contém apenas as 50 variáveis do Big 5
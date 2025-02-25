# Aplicativo Shiny de Análise de Dados

## Descrição

Este é um aplicativo interativo desenvolvido em R utilizando o framework Shiny, que permite a exploração de diferentes conjuntos de dados. O aplicativo permite que os usuários selecionem um conjunto de dados, filtrem as informações e visualizem gráficos de dispersão baseados em variáveis escolhidas.

## Funcionalidades

- **Seleção de Conjuntos de Dados**: Escolha entre os conjuntos de dados `mtcars` e `iris`.
- **Filtros Interativos**: Filtre os dados com base em múltiplas variáveis (ex: MPG para `mtcars`).
- **Visualizações**: Crie gráficos de dispersão interativos com variáveis personalizáveis.
- **Tabela de Dados**: Visualize os dados filtrados em uma tabela responsiva, que se ajusta à largura da tela.
- **Download de Dados**: Baixe os dados filtrados como um arquivo CSV.
- **Informações de Hover**: Visualize informações sobre pontos específicos no gráfico ao passar o mouse.

## Tecnologias Utilizadas

- **R**: Linguagem de programação para estatísticas e visualização de dados.
- **Shiny**: Framework para construir aplicativos web interativos em R.
- **ggplot2**: Pacote para visualização de dados.
- **DT**: Pacote para a criação de tabelas interativas.

## Como Executar

Para rodar o aplicativo, siga as instruções abaixo:

1. Certifique-se de ter o R e o RStudio instalados.
2. Instale os pacotes necessários, se ainda não estiverem instalados:

   ```R
   install.packages(c("shiny", "ggplot2", "DT"))
   
### Instruções

1. Crie um arquivo chamado `README.md` no diretório do seu projeto.
2. Copie e cole o texto acima no arquivo.
3. Salve as alterações.

Isso deve funcionar perfeitamente para descrever seu projeto em um repositório GitHub ou qualquer outro lugar onde um arquivo Markdown seja renderizado. Se precisar de mais alguma coisa, é só avisar!
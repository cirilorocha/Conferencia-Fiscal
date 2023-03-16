
# Tela de Conferência Documento Entrada Unificada

Esta é uma tela para conferência de documentos de entrada no **Totvs Protheus**, onde a mesma unifica todas as operações necessárias ao processo. Estão disponíveis as seguintes funcionalidades:

* Tela Com Todas as Informações
* Visualizar o documento de entrada
* Visualizar o PDF
* Visualizar o XML
* Editar os Livros Fiscais

A tela faz uma mescla de várias tabelas necessárias ao processo. São utilizados informações do _**Cabeçalho do Documento (SF1), Fornecedor/Cliente (SA1/SA2), itens do documento (SD1) e Livros Fiscais (SFT)**_. De forma que todos os dados necessários ao processo estejam disponíveis na mesma tela.

O processo também possui registro de auditoria de qual usuário e quando fez a conferência, além do registro também quando existe o estorno da marcação de conferência. Inclusive para os casos onde o documento é excluído e relançado, desde que mantida a mesma chave única.

O processo de visualização de XML e PDF pode ser baseado na rotina padrão para anexar arquivos do sistema **Conhecimento**, ou de um *diretório* pré-determinado em _**rede**_ ou no _**servidor**_, ou ainda via consulta de **WebService**. Caso não seja encontrado o arquivo PDF correspondente ao documento existe a possibilidade de impressão de NF-e ou CT-e a partir do XML pelo próprio sistema.

**Da forma que está atualmente ainda precisa de alguns retoques para funcione em qualquer ambiente, além do CX-Lib-Core que ainda vou publicar aqui no GitHub**

## Browser Principal

O browser principal é montado através da união das **várias tabelas** necessárias na forma de uma tabela temporária. Esta montagem é parametrizada com diversos filtros por *filial, período e status.*

O objeto foi customizado de forma a permitir todas as funcionalidades de um browse padrão do Protheus, como:

* Barra de Pesquisa
* Filtros por coluna
* Ordem, tamanho e posição das colunas
* Salvar configuração das colunas feitas diretamente com o mouse
* Filtro totalmente configurável
* Totalmente exportável para diversos formatos inclindo Excel
* Permite as operações diretamente no browse
* Possui teclas de atalho para todos os botões (alt+x ou F#)

## Tela Conferência Principal

Mostra de forma *integrada* todas as informações do browse principal em um formato mais compacto e em uma mesma tela. Todas as informações ficam disponíveis na mesma tela sem necessidade de rolagem. Exceto para os itens onde é uma *grid interativa*, ao clicar no item específico o detalhamento dos livros fiscais são atualizados na parte inferior.

Permite uma fácil acesso através dos mesmos atalhos de teclado (*alt+x* ou *F#*) do browse. Além destes existem botões para **Avançar** ou **Retroceder** o documento atualmente aberto.

### PENDÊNCIAS (TODO)

 - [ ] Converter CXPergunte para ParamBox
 - [ ] Adicionar botão para acessar os arquivos anexados (MsDocument)
 - [ ] Implementar restrição por filial do usuário
 - [ ] Função busca XML como exemplo
 - [ ] Função busca PDF como exemplo
 - [ ] Arquivos de Dicionário
 - [ ] Documentação fontes necessário
 - [ ] Salvamento das colunas do browse as vezes se comporta de forma inesperada (corrigir)

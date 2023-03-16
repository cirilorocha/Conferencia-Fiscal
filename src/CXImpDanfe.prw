#include "protheus.ch"
#include "rwmake.ch"
#include "topconn.ch"
#INCLUDE "FWPrintSetup.ch"
#INCLUDE "RPTDEF.CH"
#Include "ParmType.ch"
#include "CXInclude.ch"
#INCLUDE "CXMSPrinter.ch"
#Include "ParmType.ch"

//#############################################################################
//##+==========+============+=======+==================+======+=============+##
//##|Programa  | CXImpDanfe | Autor | Cirilo Rocha     | Data | 14/04/2022  |##
//##+==========+============+=======+==================+======+=============+##
//##|Descricao | Rotina genérica para impressão de Danfes a partir apenas do|##
//##|          |  arquivo XML                                               |##
//##|          | NÃO CONVERTER PARA TLPP POR CAUSA DA FUNÇÃO STATICCALL     |##
//##+==========+===========+================================================+##
//##|   DATA   |Programador| Manutenção Efetuada                            |##
//##+==========+===========+================================================+##
//##| 03/02/23 | Cirilo R. | Revisão final para deixar funcionando!         |##
//##|          |           |                                                |##
//##|          |           |                                                |##
//##|          |           |                                                |##
//##+==========+===========+================================================+##
//#############################################################################
User Function CXImpDanfe(cXMLNFe);	//01 cXMLNFe
							AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lRet		:= .T.			AS Logical
	Local cAviso	:= ''			AS Character
	Local cErro		:= ''			AS Character
	Local nPosIni					AS Numeric
	Local nPosFim					AS Numeric

	Private oNFe 				AS Object
	
	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cXMLNFe		AS Character		Optional

	//+-------------------+
	//|Seleciona o arquivo|
	//+-------------------+
	If ValType(cXMLNFe) == 'U'
		cArquivo := U_cGetFile(	"Todos | *.xml"					,;
								"Selecione arquivo"				,;
																,;
																,;
								.F.								,;
								GETF_LOCALHARD+GETF_NETWORKDRIVE,;
								.F.								)

		If Empty(cArquivo)	//Usuário cancelou
			Return .F.
		Endif

		If !U_CXFile(cArquivo,.T.) //Verifica se o arquivo existe e se tem conteudo
			Return .F.
		EndIf

		cXMLNFe		:= U_CXReadFile(cArquivo)
	EndIf

	//Tratamento de erros com a tag <cobr> sem <dup> dá erro na impressão do danfeii
	//NÃO É MELHOR SOLUÇÃO MAS POR ENQUANTO É O É POSSÍVEL FAZER SEM MEXER NO FONTE PADRÃO
	nPosFim	:= At('</cobr>',cXMLNFe)
	If	nPosFim > 0 .And. ;
		At('</dup>',cXMLNFe) == 0

		nPosIni	:= At('<cobr>',cXMLNFe)
		cXMLNFe	:= Left(cXMLNFe,nPosIni-1)+SubStr(cXMLNFe,nPosFim+7)	//Removo o bloco para não ocorrerem erros
	EndIf

	//Preciso usar assim porque a impressão é feita usando esse objeto mesmo
	oNFe 		:= XmlParser(cXMLNFe,"_",@cAviso,@cErro)
	
	If 	Empty(cErro)
		lRet	:= PrintDanfe()
	Else
		U_CXMsgErro('Erro ao processar o XML.'+CRLF+;
					cErro,/*cSolucao*/,/*lMsg*/,/*oArea*/)
		lRet	:= .F.
	EndIf
	
	//Libera variáveis de memória
	FreeObj(oNFe)
	oNFe := Nil

Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| PrintDanfe                      | Autor | Cirilo Rocha       | Data | 03/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Monta o objeto de impressão e faz a impressão do DANFE                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function PrintDanfe()

	//Declaracao de variaveis----------------------------------------------------------------------
//	Local aPerg					AS Array
	Local bImprime				AS CodeBlock
	Local lRet		:= .T.		AS Logical
	Local nOrdem	:= 0		AS Numeric

	//+---------------------------+
	//| Variaveis de configuracao |
	//+---------------------------+
	//Versao 3.00 2023-01-08
	Local oCfgRel				AS Object // Objeto para configuracao de relatorios

	//---------------------------------------------------------------------------------------------

	//COLOQUE AQUI AS VALIDACOES E PRE-PROCESSAMENTOS
	
	//---------------------------------------------------------------------------------------------

	//Inicializa Variaveis-------------------------------------------------------------------------
//	aPerg				:= { {"Num. Vias (x2)?" , "N", 1,  0,"Positivo() .And. NaoVazio()"} }

	//---------------------------------------------------------------------------------------------

	oCfgRel	:=	tCXRelGraf():New() //Cria objeto de configuracao
	
	oCfgRel:cNomeProg 		:= 'DANFE'						// Nome do fonte do relatorio
	oCfgRel:cTitulo 		:= "DANFE - Documento Auxiliar da Nota Fiscal Eletrônica" // Titulo do relatorio para impressao
//	oCfgRel:cPerg			:= oCfgRel:cNomeProg			// cPerg se for utilizado
//	oCfgRel:aPerg			:= aPerg						// Dados das perguntas
//	oCfgRel:bPerg			:= bPerg						// Bloco de codigo para o botao parametros
//	oCfgRel:aOrder			:= aOrdem						// Array com as ordens de impressao do relatorio
//	oCfgRel:nOrder			:= nOrdem						// Variavel que define a ordem de impressao
//	oCfgRel:lInJob			:= .T.					  		// Indica se a impressao e' vai JOB (NO SERVIDOR)
//	oCfgRel:lAdjustToLegacy	:= .T. 							// Ajuste para utilizar as mesmas coordenadas da classe TMSPrinter (COMPATIBILIDADE) (Def .F.)
	oCfgRel:_lBoxLegacy		:= .T.							// Define se os Box serao impressos pelo modo antigo, com fundo branco

//	oCfgRel:lRaw			:= .T.							// Enviar para a dispositivo de impressao caracteres binários(RAW)
//	oCfgRel:lTReport		:= .T.							// Indica que a classe foi chamada pelo TReport.
//	oCfgRel:lPDFAsPNG		:= .T.							// Indica que será gerado o PDF no formato PNG
//	oCfgRel:lDisabeSetup	:= .T.							// Inibe a tela de setup na abertua do objeto (Def .F.)
//	oCfgRel:cDirPrint		:= FwSuperGetMV('MV_RELT')		// Diretorio de impressao temporario (OPCIONAL)

//---Parametros para impressao em PDF NAO PRECISA SETAR OS PARAMETROS DE TIPO DE IMPRESSAO E DESTINACAO
//	oCfgRel:cArqRel			:= Upper(oCfgRel:cNomeProg)+'_'+DtoS(Date())+'_'+StrTran(Time(),':','') (OPCIONAL)
//	oCfgRel:cPathDest		:= cPathDest
//	oCfgRel:lPreview		:= .F.							// Desabilita o preview do PDF

	//+------------------------------+
	//| Flags do objeto FWPrintSetup |
	//+------------------------------+
//	oCfgRel:lIsTotvsPrinter	:= .F. // (Def .T.)(OPCIONAL)
//	oCfgRel:lDsOrientation	:= .F. // (Def .T.)(OPCIONAL)
//	oCfgRel:lDsPaperSize	:= .F. // (Def .T.)(OPCIONAL)
//	oCfgRel:lDsMargin		:= .F. // (Def .T.)(OPCIONAL)
//	oCfgRel:lDsDestination	:= .F. // (Def .T.)(OPCIONAL)

//	oCfgRel:cDevice			:= "SPOOL" 			// (OPCIONAL)
//	oCfgRel:nPrintType		:= IMP_SPOOL		// (OPCIONAL) //Informar este ou o acima
//	oCfgRel:nDestination	:= AMB_CLIENT		// (OPCIONAL)
	oCfgRel:nOrientation	:= nPD_RETRATO		// (OPCIONAL)
	oCfgRel:nPaperSize		:= U_CXConvPSize(DMPAPER_A4,'F') //Converte de FWMsPrinter p/ TMSPrinter // (OPCIONAL)
	oCfgRel:aMargens		:= {60,60,60,60}

	lRet	:= oCfgRel:Inicializa() //Inicializa objeto
		
	If lRet
		//	nOrdem	:= oCfgRel:nOrder
		bImprime	:= {|| lRet	:= ImpRel(nOrdem) }
		If oCfgRel:lInJob
			Eval(bImprime)
		Else
			FWMsgRun(/*oSay*/,bImprime,U_CXTxtMsg()+'Imprimindo '+oCfgRel:cTitulo,'Aguarde...')
		EndIf
	EndIf

	//Destroi os objetos tCXRelGraf,CXPrintSetup,CXMSPrinter e tCXRelGraf
	oCfgRel:Destroy()
	FwFreeVar(oCfgRel)
	
Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| ImpRel                          | Autor | Cirilo Rocha       | Data | 03/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Faz a impressão chamando o fonte padrão DanfeII                                   |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function ImpRel(nOrdem)

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aNotas			AS Array
	Local aResFisco			AS Array
	Local cModalidade		AS Character
	Local cAutoriza			AS Character
	Local cCodAutDPEC		AS Character
	Local cDtHrRecCab		AS Character
	Local dDtReceb			AS Date
	Local lImpSimp			AS Logical
	Local nTipo				AS Numeric
	
	Private nMaxItem		AS Numeric
	Private _lTimbreNFe		AS Logical	//Para não imprimir o timbre

	//Inicializa Variaveis-------------------------------------------------------------------------
	nTipo		:= 0	//Danfe normal
	lImpSimp	:= .F.	//Danfe simplificado
	cModalidade	:= '1'	//Sempre modalidade normal
	cAutoriza	:= oNFe:_nfeProc:_ProtNFE:_infProt:_nProt:TEXT
	cCodAutDPEC	:= ''
	cDtHrRecCab	:= oNFe:_nfeProc:_ProtNFE:_infProt:_dhRecbto:TEXT
	dDtReceb	:= CtoD('')
	aNotas		:= {}
	aResFisco	:= {}	//Mensagens de retorno do fisco
	
	MV_PAR04	:= 2	//Tipo de Operacao ? 2=Saida
	MV_PAR05	:= 2 	//Imprime no verso ? 2=Nao
	nMaxItem	:= 022
	_lTimbreNFe	:= .F.	//Para não imprimir o timbre

	oRpt:_lSemBmp	:= .T.	//Força não imprimir nenhuma imagem!
	oRpt:SetResolution(78)	//Tamanho estipulado para a Danfe (veio do DanfeII.prw)

	//FAÇO ESSE PROCESSO EM UMA TRANSAÇÃO PARA GARANTIR QUE O FONTE PADRÃO NÃO VAI GRAVAR NADA
	Begin Transaction

		&('StaticCall(DANFEII,IMPDET,@oRpt			,'+;
									'oNFe:_nfeProc	,'+;
									'cAutoriza		,'+;
									'cModalidade	,'+;
									'NIL			,'+;	//oNfeDPEC
									'cCodAutDPEC	,'+;
									'cDtHrRecCab	,'+;
									'dDtReceb		,'+;
									'aNotas			,'+;
									'lImpSimp		,'+;
									'nTipo			,'+;
									'aResFisco		)')
		DisarmTransaction()
	End Transaction

	oRpt:Preview()    //Visualiza antes de imprimir
	
Return

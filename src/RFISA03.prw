#INCLUDE "CRMDEF.CH"
#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'
#INCLUDE 'FWMVCDEF.CH'
#INCLUDE 'CXInclude.ch'
#include "CXnOpc.ch"
#include "CXaRotina.ch"

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| RFISA03                         | Autor | Cirilo Rocha       | Data | 27/01/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Tela de conferÍncia das informaÁıes fiscais de documentos de entrada (SF1/SD1)    |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|03/03/23| Cirilo R.| Consegui colocar uma enchoice na SFT                                   |##
//##|        |          | Adicionados campo natureza                                             |##
//##|08/03/23| Cirilo R.| Tratamento para dados sensÌveis                                        |##
//##|16/03/23| Cirilo R.| Convers„o para ParamBox                                                |##
//##|        |          | Adicionado bot„o conhecimento (MPDocument)                             |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################

////////////FILTROS RELACIONAIS COM OUTRAS TABELAS AINDA N√O EST√O FUNCIONANDO

Static cDirPDF 	:= GetMV('MX_DIRPDF' ,.F.,"F:\XML")					AS Character
Static cCompDir	:= GetMV('MX_CPLDIRP',.F.,"IIF(RTriM(cEspecie)=='CTE','DACTES\'+cAno+'\'+cMesExt+'\'+cDia,'DANFES\'+cCNPJEmp+'\'+cAno+'\'+cMesExt)")	AS Character
Static cArqPDF	:= GetMV('MX_CPLARQP',.F.,"cChave")					AS Character

Static cST_CONFERIDO	:= 'C'										AS Character
Static cST_NAOCONFE		:= 'E'										AS Character

Static lRCOMF30		:= ExistBlock('RCOMF30')						AS Logical
Static lOfuscar		:= .Not. ( VerSenha(192) .And. VerSenha(193) )	AS Logical

#Define cAlias	oBrw:Alias()	//Para simplificar o fonte

//-------------------------------------------------------------------------------------------------
User Function RFISA03()
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local aCpoSub									AS Array
	Local aParamBox		:= {}						AS Array
	Local nX										AS Numeric
    
	Private aArqTmp		:= {}						AS Array
	Private aCampos									AS Array
	Private aRtMata103								AS Array
	Private aRtMata900								AS Array
	Private aXRotina	:= {}						AS Array
	Private aMenuObs	:= {}						AS Array
	Private cCadastro	:= 'ConferÍncia Fiscal'		AS Character
	Private cDirTmp		:= GetTempPath(.T.)			AS Character	//DiretÛrio tempor·rio do cliente
	Private lTelaObs	:= .F.						AS Logical		//Controle para n„o abrir duas vezes a tela
	Private oBrw    								AS Object
	Private oDocXML		:= tCXXMLManager():New()	AS Object		//OtimizaÁ„o CXLibXMLNFeCTe
	Private oModelCache								AS Object

	//Par‚metros de filtro
	Private cFilDe									AS Character
	Private cFilAte									AS Character
	Private dLancDe									AS Date
	Private dLancAte								AS Date
	Private lSoPend									AS Logical

	//PosiÁıes array aXRotina
	Private cRX_DESC	:= 01						AS Integer
	Private bRX_EXEC	:= 02						AS Integer
	Private nRX_NOPC	:= 04						AS Integer
	Private nRX_KEY		:= 05						AS Integer
	Private nRX_VIEW	:= 06						AS Integer

	//Inicializa Variaveis-------------------------------------------------------------------------
	//Campos que n„o tem no dicion·rio coloco um equivalente no dicion·rio para funcionar
	aCpoSub	:= {{'USR_NOME'	,'AD1_DSCUSR'},;
				{'D1_VALLIQ','BAF_VLRLIQ'},;
				{'D1_RECNO' ,'CU0_RECNO'},;
				{'F1_RECNO' ,'CU0_RECNO'},;
				{'D1_TIPOCF','F0M_TPCF'};
				}

	//Campos da query, ter cuidado caso alterar a query alterar aqui tambÈm (ORDEM DOS CAMPOS)
	aCampos	:= {'F1_FILIAL' ,'F1_TIPO'   ,'F1_DOC'    ,'F1_SERIE'  ,'F1_ESPECIE','F1_EMISSAO',;
				'F1_DTDIGIT','F1_YDTLAN' ,'F1_YUSRLAN','USR_NOME'  ,'F1_FORNECE','F1_LOJA'   ,;
				'A2_NOME'   ,'A2_CGC'    ,'F1_EST'    ,'F1_VALBRUT','F1_YUSRCFG','D1_COD'    ,;
				'B1_DESC'   ,'D1_PEDIDO' ,'D1_CF'     ,'D1_TES'    ,'D1_TOTAL'  ,'D1_VALIPI' ,;
				'D1_VALDESC','D1_VALFRE' ,'D1_DESPESA','D1_SEGURO' ,'D1_DESCICM','D1_ICMSRET',;
				'D1_VALICM' ,'D1_VALLIQ' ,'D1_VALISS' ,'D1_VALPIS' ,'D1_VALCOF' ,'D1_VALCSL' ,;
				'D1_VALINS' ,'D1_VALIRR' ,'FT_VALCONT','FT_CLASFIS','FT_BASEICM','FT_VALICM' ,;
				'FT_ISENICM','FT_OUTRICM','FT_CODBCC' ,'FT_CSTPIS' ,'FT_BASEPIS','FT_VALPIS' ,;
				'FT_VALCOF' ,'D1_ITEM'	 ,'F1_CHVNFE' ,'D1_RECNO'  ,'F1_RECNO'  ,'D1_TIPOCF' ,;
				'D1_ITEMPC'	,'E2_NATUREZ','ED_DESCRIC'}
	
	//Variaveis compartilhadas de mensagens de erro
	cCodErr		:= IIf(Type('cCodErr')<>'C','',cCodErr)
	cMsgErr		:= IIf(Type('cMsgErr')<>'C','',cMsgErr)
	cSoluc		:= IIf(Type('cSoluc')<>'C','',cSoluc)
	cMsg		:= IIf(Type('cMsg')<>'C','',cMsg)

	//---------------------------------------------------------------------------------------------
	aAdd(aParamBox,{1,'Filial De' 		,'  ',,,'XM0',,020,.F.})
	aAdd(aParamBox,{1,'Filial AtÈ' 		,'ZZ',,,'XM0',,020,.T.})
	aAdd(aParamBox,{1,'Dt. LanÁ. De' 	,CtoD("01/01/08"),,,,,060,.T.})
	aAdd(aParamBox,{1,'Dt. LanÁ. AtÈ' 	,CtoD("01/01/40"),,,,,060,.T.})
	aAdd(aParamBox,{2,'Apenas N„o Conf.','Sim',{'Sim','N„o'},060,,.T.})

	If .Not. ParamBox(aParamBox,U_CXTxtMsg()+'Informe os par‚metros filtro',,,,,,,,'RFISA03',,.T.)
		Return
	EndIf

	cFilDe		:= MV_PAR01
	cFilAte		:= MV_PAR02
	dLancDe		:= MV_PAR03
	dLancAte	:= MV_PAR04
	lSoPend		:= ( MV_PAR05 == 'Sim')

	If ( dLancAte - dLancDe ) > 30
		ApMsgAlert(	'N√O … POSSÕVEL INFORMAR UM PERÕODO MAIOR QUE 30 DIAS.'+CRLF+;
					'POR QUEST’ES DE DESEMPENHO DO SISTEMA.',U_CXTxtMsg(,,.T.))
		Return
	EndIf

	//Rotinas utilizadas no browse, tela e atalhos-------------------------------------------------
	//              DescriÁ„o[1]             Rotina[2]                      nOpc[4]      Atalho[5]	View[6]
	aAdd(aXRotina,{ "Con&ferÍncia (F2)"		,{|| TelaConferencia() }		,,nOPC_VISUAL , VK_F2	,.F.})// feito assim porque n„o aceita usar 'VIEW.RFISA03'
	aAdd(aXRotina,{ "&Visualizar (F4)"		,{|| VisDoc() }					,,nOPC_VISUAL , VK_F4	,.T.})
	aAdd(aXRotina,{ "&Conferido (F9)"		,{|| GravaConf(cST_CONFERIDO) }	,,nOPC_VISUAL , VK_F9	,.T.})
	aAdd(aXRotina,{ "&Exclui Confer. (F10)"	,{|| GravaConf(cST_NAOCONFE) }	,,nOPC_VISUAL , VK_F10	,.T.})
	aAdd(aXRotina,{ "&Livros (F5)"			,{|| LivroFiscais() }			,,nOPC_ALTERA , VK_F5	,.T.})
	aAdd(aXRotina,{ "Abrir &XML (F6)"		,{|| VisXML() }					,,nOPC_VISUAL , VK_F6	,.T.})
	aAdd(aXRotina,{ "Abrir &PDF (F7)"		,{|| VisPDF() }					,,nOPC_VISUAL , VK_F7	,.T.})
	aAdd(aXRotina,{ "&Auditoria (F8)"		,{|| Auditoria() }				,,nOPC_VISUAL , VK_F8	,.T.})
	aAdd(aXRotina,{ "C&onhecimento"			,{|| Conhecimento() }			,,nOPC_VISUAL , 		,.T.})

	aAdd(aMenuObs,{"Item &SC"		,{|| ObsSC7(nOP_OBSSC,.F.) }			,,nOPC_VISUAL})
	aAdd(aMenuObs,{"Item &PC"		,{|| ObsSC7(nOP_OBSCAB,.F.) }			,,nOPC_VISUAL})
	aAdd(aMenuObs,{"T&odos SC"		,{|| ObsSC7(nOP_OBSSC,.T.) }			,,nOPC_VISUAL})
	aAdd(aMenuObs,{"&Todos PC"		,{|| ObsSC7(nOP_OBSCAB,.T.) }			,,nOPC_VISUAL})
	aAdd(aXRotina,{"&ObservaÁıes (F3)",aMenuObs								,,nOPC_VISUAL, NIL ,.F.})

	//Teclas de atalho-----------------------------------------------------------------------------
	aEval(aXRotina,{|x| IIF(x[nRX_KEY]<> NIL,SetKey(x[nRX_KEY],x[bRX_EXEC]),)})
	SetKey(VK_F3,{|| TelaObs() })

	//---------------------------------------------------------------------------------------------
	oBrw	:= tCXMBrowse():New()
	oBrw:CXSetFields(aCampos,aCpoSub)
	oBrw:SetObfuscFields({'USR_NOME','B1_DESC'})

	//Adiciona a legenda (DEIXAR A LEGENDA AQUI PARA SER A PRIMEIRA COLUNA)
	oBrw:AddLegend( "Empty(F1_YUSRCFG)"		, "BR_VERDE"	,"N√O Conferido")
	oBrw:AddLegend( ".Not.Empty(F1_YUSRCFG)", "BR_VERMELHO"	,"Conferido"	)

	oBrw:CXSetQuery(RetQuery())
	oBrw:CXBindParam({cFilDe,cFilAte,DtoS(dLancDe),DtoS(dLancAte)})
	ProcCol(oBrw)	//Processa coluna F1_TIPO porque n„o tem combo no SX3
	oBrw:CXSetQueryIndex({'F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+D1_COD+D1_ITEM'})
	//oBrw:disableDetails()
	//oBrw:disableFilter()
	//oBrw:disableSeek()
	//Adiciona filtro Relacional com outra tabela, observar o compartilhamento entre ambas as tabelas, se diferente n„o usar o campo filial.
	oBrw:_aRelation	:= {{.F.,'SF1','CabeÁalho Doc',	"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA",;
													"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA"},;
						{.F.,'SA2','Fornecedor',	"F1_FORNECE+F1_LOJA+D1_TIPOCF",;
													"A2_COD+A2_LOJA+'F'"},;
						{.F.,'SA1','Cliente'		,"F1_FORNECE+F1_LOJA+D1_TIPOCF",;
													"A1_COD+A1_LOJA+'C'"},;
						{.F.,'SFT','Livros Fiscais',"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_ITEM+'E'",;
													"FT_FILIAL+FT_NFISCAL+FT_SERIE+FT_CLIEFOR+FT_LOJA+FT_ITEM+FT_TIPOMOV"}}

	//Adiciona botıes------------------------------------------------------------------------------
	aEval(aXRotina,{|x| oBrw:AddButton(x[cRX_DESC], x[bRX_EXEC],,x[nRX_NOPC] )})
	
	oBrw:AddButton( "Legendas"		,{|| oBrw:aLegends[1][2]:View()}		,,nOPC_VISUAL )
	oBrw:AddButton( "Layout"		,U_CXBrRtCfg('oBrw')[2]					,,nOPC_ALTERA )		//ConfiguraÁıes de layout do browse

	oBrw:SetDescription(cCadastro)	//"AtualizaÁ„o de Pedidos de Venda"

	oBrw:Activate(/*oDlg*/)
	
	//Limpa arquivos tempor·rios-------------------------------------------------------------------
	For nX := 1 to Len(aArqTmp)
		fErase(aArqTmp[nX])	//N„o preciso contrar mensagens de erro porque s„o apenas tempor·rios mesmo
	Next
	FwFreeArray(aArqTmp)

	oDocXML:Destroy()
	FwFreeVar(oDocXML)

	oBrw:Destroy()
	FreeObj(oBrw)

	If oModelCache <> NIL
		oModelCache:Destroy()
		FreeObj(oModelCache)
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| TelaConferencia                 | Autor | Cirilo Rocha       | Data | 23/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Monta a tela de conferÍncia fiscal, feito desta forma porque os atalhos exigem um |##
//##|        |  bloco de cÛdigo para execuÁ„o e no menu basta a string VIEW.RFISA03, tambÈm para |##
//##|        |  proteger o atalho F2 de ser executado duas vezes e chamar uma tela por cima da   |##
//##|        |  outra causando instabilidade                                                     |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function TelaConferencia()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aEnableButtons	AS Array
	Local bKeyF2			AS CodeBlock
	Local nX				AS Numeric
	Local oKeyBak			AS Object

	//Inicializa Variaveis-------------------------------------------------------------------------
	oKeyBak	:= tCXLibFKey():New()	//Salva teclas Fx
	bKeyF2	:= SetKey(VK_F2,)	//Salva o bloco original

	//FEITO AQUI DEVIDO A UM BUG QUANDO USANDO COM O M…TODO addUserButton, SE O BOT√O EM QUEST√O N√O
	// ESTIVER VISÕVEL NA TELA O SISTEMA APRESENTA UM ERRO LOG AO ACIONAR A TECLA DE ATALHO
	SetKey(VK_F3,{|| TelaObs() })
	For nX := 1 to Len(aXRotina)
		If 	aXRotina[nX][nRX_VIEW] .And. ;
			aXRotina[nX][nRX_KEY] <> NIL

			SetKey(aXRotina[nX][nRX_KEY],aXRotina[nX][bRX_EXEC])
		EndIf
	Next
	SetKey(VK_F11,{|| DocAnterior()})
	SetKey(VK_F12,{|| ProximoDoc()})

	//---------------------------------------------------------------------------------------------

	aEnableButtons	:= {{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil},{.T.,Nil},{.T.,Nil},;
						{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil},{.F.,Nil}} //"Confirmar"###"Fechar"
	
	If oModelCache == NIL
		oModelCache := FwLoadModel("RFISA03")
	EndIf
	FWExecView('ConferÍncia',"RFISA03",,,,,,aEnableButtons,,,,oModelCache)
	oModelCache:DeActivate()	//Apenas desativo o objeto, se abrir novamente o cache fica r·pido

	oKeyBak:RestFKey()	//Restaura teclas Fx
	oBrw:Refresh()		//Atualiza a tela!

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| RetQuery                        | Autor | Cirilo Rocha       | Data | 27/01/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Monta a query com os dados para serem exibidos no browse                          |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function RetQuery()	AS Character
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local cQuery			AS Character

	//---------------------------------------------------------------------------------------------
	cQuery	:= ""
	cQuery	+= "SELECT F1_FILIAL,F1_TIPO,F1_DOC,F1_SERIE,F1_ESPECIE,F1_EMISSAO,F1_DTDIGIT,F1_YDTLAN,F1_YUSRLAN, "+CRLF
	cQuery	+= "UPPER(USR_NOME) USR_NOME,F1_FORNECE,F1_LOJA,ISNULL(A2_NOME,A1_NOME) A2_NOME, "+CRLF
	cQuery	+= "ISNULL(A2_CGC,A1_CGC) A2_CGC,F1_EST,F1_VALBRUT,F1_YUSRCFG, "+CRLF
	cQuery	+= "D1_COD,B1_DESC,D1_PEDIDO,D1_CF,D1_TES,D1_TOTAL,D1_VALIPI,D1_VALDESC,D1_VALFRE,D1_DESPESA, "+CRLF
	cQuery	+= "D1_SEGURO,D1_DESCICM,IIF(F4_INCSOL='S',D1_ICMSRET,0) D1_ICMSRET,D1_VALICM, "+CRLF
	cQuery	+= "D1_TOTAL+D1_VALFRE+D1_VALIPI+D1_DESPESA+D1_SEGURO+IIF(F4_INCSOL='S',D1_ICMSRET,0)-D1_VALDESC-D1_DESCICM D1_VALLIQ, "+CRLF
	cQuery	+= "D1_VALISS,D1_VALPIS,D1_VALCOF,D1_VALCSL,D1_VALINS,D1_VALIRR, "+CRLF
	cQuery	+= "FT_VALCONT,FT_CLASFIS,FT_BASEICM,FT_VALICM,FT_ISENICM,FT_OUTRICM,FT_CODBCC,FT_CSTPIS,"+CRLF
	cQuery	+= "FT_BASEPIS,FT_VALPIS,FT_VALCOF,D1_ITEM,F1_CHVNFE,SD1.R_E_C_N_O_ D1_RECNO,SF1.R_E_C_N_O_ F1_RECNO, "+CRLF
	cQuery	+= "IIF(A2_NOME IS NULL,'C','F') D1_TIPOCF,D1_ITEMPC,E2_NATUREZ,ED_DESCRIC "+CRLF
	cQuery	+= "FROM "+FWSX2Util():GetFile('SD1')+" SD1 "+CRLF
	//CabeÁalho Doc. Entrada
	cQuery	+= "INNER JOIN "+FWSX2Util():GetFile('SF1')+" SF1 "+CRLF
	cQuery	+= "	ON SF1.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND F1_FILIAL	= D1_FILIAL "+CRLF
	cQuery	+= "	AND F1_DOC		= D1_DOC "+CRLF
	cQuery	+= "	AND F1_SERIE	= D1_SERIE "+CRLF
	cQuery	+= "	AND F1_FORNECE	= D1_FORNECE "+CRLF
	cQuery	+= "	AND F1_LOJA		= D1_LOJA "+CRLF
	//Produtos
	cQuery	+= "INNER JOIN "+FWSX2Util():GetFile('SB1')+" SB1 "+CRLF
	cQuery	+= "	ON SB1.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND "+FWJoinFilial('SD1','SB1',,,.F.,,.T.)+CRLF //Retorna a expressao SQL para ser utilizada em inner join para relacionar duas tabelas
	cQuery	+= "	AND B1_COD		= D1_COD "+CRLF
	//TES
	cQuery	+= "INNER JOIN "+FWSX2Util():GetFile('SF4')+" SF4 "+CRLF
	cQuery	+= "	ON SF4.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND "+FWJoinFilial('SD1','SF4',,,.F.,,.T.)+CRLF //Retorna a expressao SQL para ser utilizada em inner join para relacionar duas tabelas
	cQuery	+= "	AND F4_CODIGO = D1_TES "+CRLF
	//Itens Livro Fiscal
	cQuery	+= "FULL OUTER JOIN "+FWSX2Util():GetFile('SFT')+" SFT "+CRLF
	cQuery	+= "	ON SFT.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND FT_FILIAL	= D1_FILIAL "+CRLF
	cQuery	+= "	AND FT_NFISCAL	= D1_DOC "+CRLF
	cQuery	+= "	AND FT_SERIE	= D1_SERIE "+CRLF
	cQuery	+= "	AND FT_CLIEFOR	= D1_FORNECE "+CRLF
	cQuery	+= "	AND FT_LOJA		= D1_LOJA "+CRLF
	cQuery	+= "	AND FT_ITEM		= D1_ITEM "+CRLF
	cQuery	+= "	AND FT_TIPOMOV	= 'E' "+CRLF
	//Usu·rios
	cQuery	+= "FULL OUTER JOIN SYS_USR AS SU "+CRLF
	cQuery	+= "	ON SU.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND USR_ID		= F1_YUSRLAN "+CRLF
	//Clientes
	cQuery	+= "FULL OUTER JOIN "+FWSX2Util():GetFile('SA1')+" SA1 "+CRLF
	cQuery	+= "	ON SA1.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND "+FWJoinFilial('SD1','SA1',,,.F.,,.T.)+CRLF //Retorna a expressao SQL para ser utilizada em inner join para relacionar duas tabelas
	cQuery	+= "	AND A1_COD		= D1_FORNECE "+CRLF
	cQuery	+= "	AND A1_LOJA		= D1_LOJA "+CRLF
	cQuery	+= "	AND F1_TIPO IN ('D','B') "+CRLF	//Vinculado a Cliente
	//Fornecedores
	cQuery	+= "FULL OUTER JOIN "+FWSX2Util():GetFile('SA2')+" SA2 "+CRLF
	cQuery	+= "	ON SA2.D_E_L_E_T_='' "+CRLF
	cQuery	+= "	AND "+FWJoinFilial('SD1','SA2',,,.F.,,.T.)+CRLF //Retorna a expressao SQL para ser utilizada em inner join para relacionar duas tabelas
	cQuery	+= "	AND A2_COD		= D1_FORNECE "+CRLF
	cQuery	+= "	AND A2_LOJA		= D1_LOJA "+CRLF
	cQuery	+= "	AND F1_TIPO NOT IN ('D','B') "+CRLF	//Vinculado a Fornecedor
	//Natureza financeira SE2+SED
	cQuery	+= "OUTER APPLY ( "+CRLF
	cQuery	+= "	SELECT TOP 1 E2_NATUREZ,ED_DESCRIC "+CRLF
	cQuery	+= "	FROM "+FWSX2Util():GetFile('SE2')+" SE2 "+CRLF
	cQuery	+= "	INNER JOIN "+FWSX2Util():GetFile('SED')+" SED "+CRLF
	cQuery	+= "		ON SED.D_E_L_E_T_='' "+CRLF
	cQuery	+= "		AND "+FWJoinFilial('SE2','SED',,,.F.,,.T.)+CRLF	//Retorna a expressao SQL para ser utilizada em inner join para relacionar duas tabelas
	cQuery	+= "		AND ED_CODIGO = E2_NATUREZ "+CRLF
	cQuery	+= "	WHERE SE2.D_E_L_E_T_='' "+CRLF
	cQuery	+= "		AND E2_FILIAL	= D1_FILIAL "+CRLF
	cQuery	+= "		AND E2_PREFIXO	= D1_SERIE "+CRLF
	cQuery	+= "		AND E2_NUM		= D1_DOC "+CRLF
	cQuery	+= "		AND E2_FORNECE	= D1_FORNECE "+CRLF
	cQuery	+= "		AND E2_LOJA		= D1_LOJA "+CRLF
	cQuery	+= "		AND E2_ORIGEM	= 'MATA100' "+CRLF
	cQuery	+= "	ORDER BY SE2.R_E_C_N_O_ "+CRLF
	cQuery	+= ") CP "+CRLF
	//Filtros
	cQuery	+= "WHERE SD1.D_E_L_E_T_ = '' "+CRLF
	cQuery	+= "	AND D1_FILIAL BETWEEN ? AND ? "+CRLF
	cQuery	+= "	AND F1_YDTLAN BETWEEN ? AND ? "+CRLF
	If lSoPend
		cQuery	+= "	AND F1_YUSRCFG = '' "+CRLF
	EndIF
	cQuery	+= "	AND D1_FORMUL <> 'S' "+CRLF

Return cQuery

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| ProcCol                         | Autor | Cirilo Rocha       | Data | 27/01/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Ajusta a coluna F1_TIPO porque a mesma n„o tem combo no dicion·rio SX3            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function ProcCol(oBrw)
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local nPos				AS Numeric
	Local nTamCpo			AS Numeric

	//O campo F1_TIPO n„o tem combo cadastrado, ent„o alimento manualmente aqui
	nPos		:= aScan(oBrw:aColumns,{|x| x:GetID() == 'F1_TIPO' })
	If 	nPos > 0 .And. ;
		Len(oBrw:aColumns[nPos]:GetOptions()) == 0

		nTamCpo	:= oBrw:aColumns[nPos]:GetSize()
		aCombo	:= U_CXTipoNF(2)
		aEval(aCombo,{|x| IIf(Len(x) > nTamCpo,nTamCpo:=Len(x),) } )

		oBrw:aColumns[nPos]:SetOptions(U_CXTipoNF(4))
		oBrw:aColumns[nPos]:SetSize( nTamCpo )
	EndIf

	//Mostro o campo de forma mais amig·vel ao usu·rio
	nPos		:= aScan(oBrw:aColumns,{|x| x:GetID() == 'F1_YUSRCFG' })
	If 	nPos > 0

		nTamCpo	:= 40
		oBrw:aColumns[nPos]:SetData( {|| U_RFISA03A(F1_YUSRCFG) } )
		oBrw:aColumns[nPos]:SetSize( nTamCpo )
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| VisDoc                          | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Abre a tela de visualizaÁ„o de documento de entrada                               |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function VisDoc()
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local oArea         AS Object

	Private aRotina		AS Array

	//---------------------------------------------------------------------------------------------
	oArea	:= tCtrlAlias():GetArea({'SF1','SD1','SM0','#KEY'})
	If aRtMata103 == NIL
		aRtMata103	:= FWLoadMenuDef('MATA103')
	EndIf
	aRotina	:= aClone(aRtMata103)
	
	If PosSF1(	(cAlias)->F1_FILIAL	,;
				(cAlias)->F1_DOC	,;
				(cAlias)->F1_SERIE	,;
				(cAlias)->F1_FORNECE,;
				(cAlias)->F1_LOJA	)

		cFilAnt	:= SF1->F1_FILIAL
		FWSM0Util():setSM0PositionBycFilAnt()

		A103NFiscal('SF1',SF1->(Recno()),nOPC_VISUAL)
	EndIf
	oArea:RestArea()		//Restaura area
	oArea:Destroy()
	FWFreeVar(oArea)

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| PosSF1                          | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Posiciona no registro do documento de entrada (SF1) correspondente                |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function PosSF1(	cFilDoc	,;	//01 cFilDoc
						cNumDoc	,;	//02 cNumDoc
						cSerDoc	,;	//03 cSerDoc
						cFornec	,;	//04 cFornec
						cLoja	);	//05 cLoja
							AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lRet				AS Logical
	
	//---------------------------------------------------------------------------------------------
	SF1->(U_CXSetOrd(1))	//F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_TIPO
	lRet	:= SF1->(dbSeek(cFilDoc+cNumDoc+cSerDoc+cFornec+cLoja))
	If .Not. lRet
		U_CXMsgErro('Erro ao localizar documento de entrada (SD1): '+cFilDoc+'-'+cNumDoc+'-'+;
					cSerDoc+'-'+cFornec+'-'+cLoja,;
					'INFORME O SETOR DE T.I.',/*lMsg*/,/*oArea*/)
	EndIf

Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| LivroFiscais                    | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Abre a tela de manutenÁ„o dos livros fiscais por item                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function LivroFiscais()
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local lAtualizou	:= .F.	AS Logical
	Local oArea         		AS Object
	Local nOpc					AS Numeric

	Private aRotina				AS Array
	Private ALTERA	:= .T.		AS Logical	//Preciso sobrepor as vari·veis da rotina chamadora
	Private INCLUI	:= .F.		AS Logical

	//---------------------------------------------------------------------------------------------
	oArea	:= tCtrlAlias():GetArea({'SF3','SFT','SM0','#KEY'})
	If aRtMata900 == NIL
		aRtMata900	:= FWLoadMenuDef('aRtMata900')
	EndIf
	aRotina	:= aClone(aRtMata900)
	nOpc	:= nOPC_ALTGRD
	//6 = Alteracao sem a permissao para incluir novas linhas. Eh valido apenas para os objetos GetDados e GetDb

	//Documento j· conferido
	If .Not. Empty((cAlias)->F1_YUSRCFG)
		//If .Not. ApMsgNoYes('Este documento j· est· marcado como conferido.'+CRLF+;
		//					'N√O SER¡ POSSÕVEL ALTERAR OS DADOS DO LIVRO FISCAL.'+CRLF+CRLF+;
		//					'DESEJA ABRIR NO MODO SOMENTE VISUALIZA«√O ?',U_CXTxtMsg(,,.T.))
		//	Return
		//Else
		//	nOpc	:= nOPC_VISUAL
		//	ALTERA	:= .F.
		//EndIf
		//A ROTINA PADR√O N√O EXISTE ESSA OPERA«√O, ELE SEMPRE ALTERA
		U_CXHelp(,,	'Este documento j· est· marcado como conferido.'+CRLF+;
					'N√O SER¡ POSSÕVEL ALTERAR OS DADOS DO LIVRO FISCAL.',,/*cSolucao*/)
		Return
	EndIf
	
	If PosSF3(	(cAlias)->F1_FILIAL	,;
				(cAlias)->F1_DOC	,;
				(cAlias)->F1_SERIE	,;
				(cAlias)->F1_FORNECE,;
				(cAlias)->F1_LOJA	)

		cFilAnt	:= SF3->F3_FILIAL
		FWSM0Util():setSM0PositionBycFilAnt()

		SFT->(DbSetOrder (3))	//FT_FILIAL+FT_TIPOMOV+FT_CLIEFOR+FT_LOJA+FT_SERIE+FT_NFISCAL+FT_IDENTF3
		If 	.Not. ( AllTrim(SF3->F3_CFO) == '999' ) .And. ;
			SFT->(MsSeek(FWxFilial("SFT")+'E'+SF3->(F3_CLIEFOR+F3_LOJA+F3_SERIE+F3_NFISCAL+F3_IDENTFT)))

			MATA917('SF3',SF3->(Recno()),nOpc)	//Esta funÁ„o retorna status se clicou em OK alterando!
			
			If nOpc	== nOPC_ALTGRD
				AtualizaTmp({'SFT'})
				lAtualizou	:= .T.
			EndIf
		Else
			xMagHelpFis (U_CXTxtMsg()+"AtenÁ„o - Processo interrompido",;
			FWI18NLANG("MATA917","STR0016",0016),;	//"O Acerto Fiscal Por Item somente È permitido para Notas Fiscais que possuam escrituraÁ„o fiscal por item (tabela SFT) relacionado, ou seja, que tenha vindo de um processo completo, COMPRAS ou FATURAMENTO. Neste caso, n„o foi encontrado tal relacionamento n„o premitindo a execuÁ„o da opÁ„o solicitada."
			FWI18NLANG("MATA917","STR0017",0017)) 	//"A tabela de Livros Fiscais por item (SFT) necessita dos itens da Nota Fiscal (Tabela SD1 ou SD2) que neste caso n„o existe, pois esta Nota Fiscal pode ter sido incluÌda manualmente atravÈs de acertos fiscais onde n„o ser· possÌvel tal manutenÁ„o devido somente existir informaÁıes na tabela SF3 (Livros Fiscais)."
		EndIf
	EndIf

	oArea:RestArea()		//Restaura area
	oArea:Destroy()
	FWFreeVar(oArea)

	If	lAtualizou .And. ;
		FWIsInCallStack('TELACONFERENCIA')	//Veio da tela de conferÍncia preciso atualizar

		oModelCache:DeActivate()			//ForÁa a recarga dos dados
		oModelCache:Activate()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| PosSF3                          | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Posiciona no registro dos livros fiscais (SF3) correspondente                     |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function PosSF3(	cFilDoc	,;	//01 cFilDoc
						cNumDoc	,;	//02 cNumDoc
						cSerDoc	,;	//03 cSerDoc
						cFornec	,;	//04 cFornec
						cLoja	);	//05 cLoja
							AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lRet				AS Logical
	
	//---------------------------------------------------------------------------------------------
	SF3->(U_CXSetOrd(5))	//F3_FILIAL+F3_SERIE+F3_NFISCAL+F3_CLIEFOR+F3_LOJA+F3_IDENTFT
	lRet	:= SF3->(dbSeek(cFilDoc+cSerDoc+cNumDoc+cFornec+cLoja))
	If .Not. lRet
		U_CXMsgErro('Erro ao localizar os livros fiscais (SF3): '+cFilDoc+'-'+cNumDoc+'-'+;
					cSerDoc+'-'+cFornec+'-'+cLoja+CRLF+;
					'DOCUMENTO N√O GEROU LIVROS FISCAIS.',;
					/*cSolucao*/,/*lMsg*/,/*oArea*/)
	EndIf

Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| AtualizaTmp                     | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Grava dados atualizados nas tabelas origem para a tabela tempor·ria da tela       |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function AtualizaTmp(aAlias)

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aAreas		AS Array
	Local nX,nY			AS Numeric
	Local oArea         AS Object

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aAlias		AS Array

	//---------------------------------------------------------------------------------------------
	cChave	:= 	(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
	
	aAreas	:= aAlias
	aAdd(aAreas,cAlias)
	oArea	:= tCtrlAlias():GetArea(aAreas)

	For nX := 1 to Len(aAlias)
		If aAlias[nX] == 'SF1'
			SF1->(U_CXSetOrd(1))	//F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_TIPO
		ElseIf aAlias[nX] == 'SD1'
			SD1->(U_CXSetOrd(1))	//D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA+D1_COD+D1_ITEM
		ElseIf aAlias[nX] == 'SFT'
			SFT->(U_CXSetOrd(1))	//FT_FILIAL+FT_TIPOMOV+FT_SERIE+FT_NFISCAL+FT_CLIEFOR+FT_LOJA+FT_ITEM
		EndIf

		(cAlias)->(dbSeek(cChave))	//Posiciono no primeiro registro-------------------------------
		While 	(cAlias)->(!EOF()) .And. ;
				(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChave

			//Posiciono a tabela referido
			If aAlias[nX] == 'SF1'
				SF1->(dbSeek((cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)))
			ElseIf aAlias[nX] == 'SD1'
				SD1->(dbSeek((cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+D1_COD+D1_ITEM)))
			ElseIf aAlias[nX] == 'SFT'
				SFT->(dbSeek((cAlias)->(F1_FILIAL+'E'+F1_SERIE+F1_DOC+F1_FORNECE+F1_LOJA+D1_ITEM)))
			EndIf

			//Atualiza campos na tabela tempor·ria-------------------------------------------------
			If (aAlias[nX])->(Found())
				RecLock(cAlias,.F.)
					For nY := 1 to Len(aCampos)
						If FwTabPref(aCampos[nY]) == aAlias[nX]
							U_CXFieldPut(aCampos[nY],U_CXFieldGet(aCampos[nY],aAlias[nX]),cAlias)
						EndIf
					Next
				(cAlias)->(MsUnLock())
			Else
				U_CXMsgErro('Erro ao localizar a tabela '+aAlias[nX]+'-'+FwSX2Util():GetX2Name(aAlias[nX]),;
							'INFORME AO SETOR DE T.I.',/*lMsg*/,/*oArea*/)
			EndIf

			(cAlias)->(dbSkip())
		EndDo
	Next

	oArea:RestArea()		//Restaura area
	oArea:Destroy()
	FWFreeVar(oArea)
	oBrw:Refresh()			//Atualiza a tela!

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| GetXML                          | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | FunÁ„o para obter o arquivo XML do documento no sistema                           |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function GetXML(	lMsg	);	//01 lMsg
							AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aArq				AS Array
	Local cFile	:= ''		AS Character
	Local cXML	:= ''		AS Character
	Local bVldArq			AS CodeBlock
	Local nX				AS Numeric
	Local oArea         	AS Object

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lMsg		AS Logical				Optional Default .T.

	//Inicializa Variaveis-------------------------------------------------------------------------
	If ExistBlock('CXVLDXML')	//CXLibXMLNFeCTe
		bVldArq	:= {|cArq| U_CXVldXML(	cArq			,;	//01 Nome do arquivo
										SF1->F1_DOC		,;	//02 Numero do documento
										SF1->F1_SERIE	,;	//03 Serie do Documento
										SF1->F1_FORNECE	,;	//04 Fornecedor/Cliente
										SF1->F1_LOJA	,;	//05 Loja do Fornecedor/Cliente
										SF1->F1_ESPECIE	,;	//06 Especie do documento
										SF1->F1_EMISSAO	,;	//07 Data de emissao
										SF1->F1_TIPO	,;	//08 Tipo do documento
										SF1->F1_EST		,;	//09 Estado
										.F.				,;	//10 Mostra mensagens de erro (def .F.)
										/*aNFVinc*/		,;	//11 Notas fiscais vinculadas ao CTe (FUTURO) (def {} )
										/*lTeste*/		,;	//12 Modo de teste (def .F.)
										/*lVldTot*/		,;	//13 Valida valores do documento (def .F.)
										/*bPreVldT*/	,;	//14 Bloco de pre-validacao dos totais (def vazio)
										/*nVlDcVin*/	,;	//15 Valida documentos vinculados (def MX_VLDXML1) (0=Apenas Alerta;1=Obrigatorio)
										oDocXML			);	//16 Object cache para otimizaÁ„o (def NIL)
										}
	Else
		bVldArq	:= {|| .T.}	//DEMONSTRA«√O PEGA QUALQUER XML ANEXADO N√O FAZ UMA REAVALIA«√O SE O ARQUIVO EST¡ CORRETO
	EndIf

	//Apenas estas espÈcies tem XML
	If AllTrim((cAlias)->F1_ESPECIE) $ 'CTE/SPED'
		oArea		:= tCtrlAlias():GetArea({'SM0'})
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf
		
		SF1->(dbGoTo((cAlias)->F1_RECNO))

		//Busca nos arquivos anexados do sistema (Conhecimento/MPDocument)
		If ExistBlock('CXGETDOC')	//CXLibMsDoc
			aArq	:= U_CXGetDoc('SF1',.F.)
			For nX := 1 to len(aArq)
				If 	Upper(Right(AllTrim(aArq[nX]),4)) == '.XML' .And. ;	//Se o tipo do arquivo e' XML
					U_CXFile( AllTrim(aArq[nX]) , .T. )					//Busca o arquivo fisico no servidor

					//Verifico se o arquivo È mesmo v·lido
					If eVal(bVldArq,aArq[nX])
						cFile	:= aArq[nX]
						Exit
					EndIf
				EndIf
			Next

			If Empty(cFile)			//N„o achou nos arquivos anexados
				//Busca na pasta do XML salvo
				If U_CXBscXML(	(cAlias)->F1_CHVNFE	,;	//01 cChave
								@cFile				,;	//02 @cFile
								(cAlias)->F1_EMISSAO)	//03 data de emiss„o
					If .Not. eVal(bVldArq,cFile)	//Valido o arquivo por precauÁ„o
						cFile	:= ''
					EndIf
				Else
					cFile	:= ''
				EndIf
			EndIf
		EndIf

		If Empty(cFile)
			//Busca pela integraÁ„o com o SIEG
			cXML	:= ''
			If lRCOMF30
				cXML	:= U_RCOMF30((cAlias)->F1_CHVNFE,(cAlias)->F1_ESPECIE,lMsg)
			EndIf
			
			////////////////////////////////////////////////////////////////////////////////////
			//////////////////////////Modo desmonstraÁ„o apenas/////////////////////////////////
			////////////////////////////////////////////////////////////////////////////////////
			If Empty(cXML)
				If AllTrim((cAlias)->F1_ESPECIE) $ 'SPED'
					cXML	:= XMLNFEDemo()
				Else
					cXML	:= XMLCTEDemo()
				EndIf
			EndIf

			If Empty(cXML)
				U_CXMsgErro('Erro ao localizar o arquivo XML do documento.',/*cSolucao*/,lMsg,/*oArea*/)
			EndIf
		Else
			cXML	:= U_CXReadFile(cFile,lMsg)
		EndIf

		oArea:RestArea()		//Restaura area
		oArea:Destroy()
		FWFreeVar(oArea)
	Else
		U_CXMsgErro('A espÈcie '+(cAlias)->F1_ESPECIE+' do documento n„o possui XML.',;
					/*cSolucao*/,lMsg,/*oArea*/)
	EndIf

Return cXML

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| VisXML                          | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Visualizar o arquivo XML do documento                                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function VisXML()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cXML			AS Character
	Local cArquivo		AS Character

	//---------------------------------------------------------------------------------------------
	cXML	:= GetXML()

	If .Not. Empty(cXML)
		cArquivo	:= cDirTmp+NfeIdSPED(cXML,'Id')+'.xml'
		If U_CXWriteFile(cArquivo,cXML)
			aAdd(aArqTmp,cArquivo)	//Guardo os arquivos tempor·rios para posterior limpeza
			ShellExecute("open",cArquivo,"","",1)
		EndIf
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| VisPDF                          | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Visualizar o arquivo PDF do documento                                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
//F:/XML/DANFES/08348609000168/2022/Setembro/23220906891105000679550010001795231151631378.pdf
//F:/XML/DACTES/2022/Setembro/23/23220929303092000160570010000569211672385819.pdf
//------------------------------------------------------------------------------------------------
Static Function VisPDF()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aFiles		AS Array
	Local cFile			AS Character
	Local cChave		AS Character
	Local cDirPDFCmp	AS Character
	Local lAchou		AS Logical
	Local nArq			AS Numeric
	Local oArea			AS Object

	Local cAno			AS Character
	Local cMes			AS Character
	Local cDia			AS Character
	Local cMesExt		AS Character
	Local cCNPJEmit		AS Character
	Local cCNPJEmp		AS Character
	Local cEspecie		AS Character

	//Inicializa Variaveis-------------------------------------------------------------------------
	lAchou		:= .F.

	//Apenas estas espÈcies tem XML
	If AllTrim((cAlias)->F1_ESPECIE) $ 'CTE/SPED'

		oArea		:= tCtrlAlias():GetArea({'SM0'})
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		//Busca no diretÛrio onde os arquivos PDF ficam salvos-----------------------------------------
		cChave		:= AllTrim((cAlias)->F1_CHVNFE)
		cEspecie	:= IIF(SubStr(cChave,21,2)=='55','SPED','CTE')

		cAno		:= '20'+SubStr(cChave,3,2)
		cMes		:= SubStr(cChave,5,2)
		cDia		:= LTrim(Str(Day((cAlias)->F1_EMISSAO)))
		cCNPJEmit	:= SubStr(cChave,7,14)
		cMesExt		:= MesExtenso(Val(cMes))
		cCNPJEmp	:= SM0->M0_CGC

		cDirPDFCmp	:= cDirPDF+'\'+&(cCompDir)+'\'
		If .Not. ExistDir(cDirPDFCmp)
			cDirPDFCmp	:= cDirPDF+'\'
			If .Not. ExistDir(cDirPDFCmp)
				cDirPDFCmp	:= 'c:\'
			EndIf
		EndIf
		cDirPDFCmp	:= StrTran(cDirPDFCmp,'\\','\')

		cFile	:= cDirPDFCmp+'\'+&(cArqPDF)+'.PDF'
		If U_CXFile(cFile)
			lAchou	:= .T.
		Else
			aFiles 	:= Directory(cDirPDFCmp+"\*"+cChave+"*.PDF")

			For nArq := 1 To Len(aFiles)
				cFile 	:= AllTrim(cDirPDFCmp+aFiles[nArq,1])

				//Busca um arquivo com o mesmo nome da chave
				If cChave $ cFile
					lAchou	:= .T.
					Exit
				EndIf
			Next
		EndIf

		//Gera o PDF a partir do XML-------------------------------------------------------------------
		If .Not. lAchou
			ApMsgAlert(	'N√O foi possÌvel encontrar o arquivo PDF salvo na pasta configurada:'+CRLF+;
						cDirPDFCmp+CRLF+CRLF+;
						'SER¡ FEITA UMA TENTATIVA DE IMPRESS√O A PARTIR DO XML.',U_CXTxtMsg(,,.T.))
			
			cXML	:= GetXML()

			If .Not. Empty(cXML)
				If cEspecie = 'SPED'
					U_CXImpDanfe(cXML)
				Else
					U_CXImpDacte(cXML)
				EndIf
			EndIf
		Else
			ShellExecute("open",cFile,"","",1)
		EndIf
		oArea:RestArea()		//Restaura area
		oArea:Destroy()
		FWFreeVar(oArea)
	Else
		If ApMsgYesNo(	'A espÈcie '+(cAlias)->F1_ESPECIE+' do documento n„o È NF-e ou CT-e.'+CRLF+;
						'DESEJA ABRIR OS ARQUIVOS ANEXADOS NO CONHECIMENTO ?',U_CXTxtMsg(,,.T.))
			Conhecimento()
		EndIf
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| GravaConf                       | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Rotina para efetuar a gravaÁ„o da flag de conferÍncia e sua auditoria             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function GravaConf(cOper);	//01 cOper
							AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cChave					AS Character
	Local lConferido				AS Logical
	Local lRet			:= .T.		AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cOper		AS Character

	//PrÈ-ValidaÁ„o do registro--------------------------------------------------------------------
	lConferido	:= ( .Not. Empty((cAlias)->F1_YUSRCFG) )
	If 	.Not. lConferido .And. ;
		(cOper==cST_NAOCONFE)
	
		U_CXMsgErro('Status do documento j· est· N√O conferido.',;
					/*cSoluc*/,/*lMsg*/,/*oArea*/)
		Return .F.
	ElseIf	lConferido .And. ;
			(cOper==cST_CONFERIDO)

		U_CXMsgErro('Status do documento j· est· CONFERIDO.',;
					/*cSoluc*/,/*lMsg*/,/*oArea*/)
		Return .F.
	EndIf

	//---------------------------------------------------------------------------------------------
	Begin Transaction
		SF1->(dbGoTo((cAlias)->F1_RECNO))
		If .Not. RecLock('SF1',.F.)	//Tento locar o registro por seguranÁa
			U_CXMsgErro('Erro ao bloquear registro '+LTrim(Str((cAlias)->F1_RECNO))+' na tabela SF1.',;
						/*cSolucao*/,/*lMsg*/,/*oArea*/)
			lRet	:= .F.
		EndIf

		If lRet
			lConferido	:= ( .Not. Empty(SF1->F1_YUSRCFG) )

			If 	.Not. lConferido .And. ;
				(cOper==cST_NAOCONFE)
				
				DisarmTransaction()
				SF1->(MsUnLock())
				U_CXMsgErro('Problema no processamento, status do documento j· est· N√O conferido.',;
							'VERIFIQUE SE OUTRO USU¡RIO TAMB…M EST¡ TRATANDO ESSE MESMO DOCUMENTO.',/*lMsg*/,/*oArea*/)
				lRet	:= .F.
			ElseIf	lConferido .And. ;
					(cOper==cST_CONFERIDO)

				DisarmTransaction()
				SF1->(MsUnLock())
				U_CXMsgErro('Problema no processamento, status do documento j· est· CONFERIDO.',;
							'VERIFIQUE SE OUTRO USU¡RIO TAMB…M EST¡ TRATANDO ESSE MESMO DOCUMENTO.',/*lMsg*/,/*oArea*/)
				lRet	:= .F.
			EndIf
		EndIf

		If lRet
			If cOper == cST_CONFERIDO
				SF1->F1_YUSRCFG	:= __cUserID+DtoS(Date())
			Else
				SF1->F1_YUSRCFG	:= ''
			EndIf
			
			//GravaÁ„o do campo na tabela tempor·ria---------------------------------------------------
			cChave	:= 	(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
			(cAlias)->(dbSeek(cChave))
			While 	(cAlias)->(!EOF()) .And. ;
					(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChave

				RecLock(cAlias,.F.)
					(cAlias)->F1_YUSRCFG	:= SF1->F1_YUSRCFG
				(cAlias)->(MsUnLock())
				
				(cAlias)->(dbSkip())
			EndDo

			//Grava auditoria----------------------------------------------------------------------
			RecLock('Z05',.T.)
				Z05->Z05_FILIAL	:= SF1->F1_FILIAL
				Z05->Z05_CODUSR	:= __cUserID
				Z05->Z05_DATA	:= Date()
				Z05->Z05_HORA	:= Time()
				Z05->Z05_OPERAC	:= cOper
				Z05->Z05_CHVDOC	:= SF1->(F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
			Z05->(MsUnLock())
		EndIf
	End Transaction
	
	If FWIsInCallStack('TELACONFERENCIA') //Veio da tela de conferÍncia preciso atualizar o campo F1_YUSRCFG
		oModelCache:LoadValue( 'MODEL_SF1' , 'F1_YUSRCFG' , Left(U_RFISA03A(SF1->F1_YUSRCFG),35) ) //Preenche o campo, sem validacoes ou gatilhos
		AtualizaTelaMVC()		//forca a atualizacao da tela
	Else
		oBrw:Refresh()			//Atualiza a tela!
	EndIf

Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| RFISA03A                        | Autor | Cirilo Rocha       | Data | 13/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Monta o texto para exibiÁ„o do usu·io que fez a conferÍncia.                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
User Function RFISA03A(cConteudo);	//01 cConteudo
							AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cRet	:= ''			AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cConteudo		AS Character

	If .Not. Empty(cConteudo)
		cRet	+= DtoC(StoD(SubStr(cConteudo,7,8)))+'-'+U_CXNmUsr(Left(cConteudo,6),.T.)
	EndIf
	
Return cRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| RFISA03B                        | Autor | Cirilo Rocha       | Data | 24/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | FunÁ„o usada para restriÁ„o do menu dos livros fiscais                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
User Function RFISA03B(cAliasFis, nRecno, nOpc)	//01 cConteudo

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lProssegue	:= .T.		AS Logical
	Local cSufixo					AS Character
	Local oArea         			AS Object
	Local uRet						AS Variant

	//---------------------------------------------------------------------------------------------
	If SF3->F3_CFO < '5000'				//Por enquanto È apenas para documentos de entrada
		oArea		:= tCtrlAlias():GetArea({'SF1'})
		SF1->(dbSetOrder(1))	//F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_TIPO
		If SF1->(dbSeek(FwXFilial('SF1')+SF3->(F3_NFISCAL+F3_SERIE+F3_CLIEFOR+F3_LOJA)))
			If .Not. Empty(SF1->F1_YUSRCFG)
				U_CXHelp(,,'Este documento j· foi marcado como conferido (F1_YUSRCFG):'+CRLF+;
							U_RFISA03A(SF1->F1_YUSRCFG),,;
							'CASO NECESS¡RIO REMOVA O VISTO ANTES DE EFETUAR ALTERA«’ES/EXCLUS’ES.')
				lProssegue	:= .F.
			EndIf
		Else
			U_CXHelp(,,	'Erro ao localizar o documento de entrada vinculado a este registros:'+CRLF+;
						'Chave: '+FwXFilial('SF1')+'-'+SF3->(F3_NFISCAL+'-'+F3_SERIE+'-'+F3_CLIEFOR+'-'+F3_LOJA),,;
						'INFORME O SETOR DE T.I.')
			lProssegue	:= .F.
		EndIf
		oArea:RestArea()		//Restaura area
		oArea:Destroy()
		FWFreeVar(oArea)
	EndIf

	If Type('_aRotMA900') == 'U'	//Vari·vel de controle
		lProssegue	:= .F.
		U_CXHelp(,,	'Erro interno, vari·vel _aRotMA900 n„o encontrada.'+CRLF+;
					'Deveria ser gerada no P.E. MA900MNU.',,;
					'INFORME O SETOR DE T.I.')
	ElseIf nOpc == NIL
		lProssegue	:= .F.
		U_CXHelp(,,	'Erro interno, vari·vel nOpc n„o foi recebida.',,;
					'INFORME O SETOR DE T.I.')
	EndIf

	If lProssegue
		cComando	:= _aRotMA900[nOpc][nRT_FUNC]
		If At('(',cComando,/*nPosIni*/) == 0	// buscar texto da esquerda para a direita
			cSufixo	:= ''
			If cAliasFis <> NIL
				cSufixo	+= "'"+cAliasFis+"'"
			EndIf
			cSufixo	+= ','
			If nRecno <> NIL
				cSufixo	+= LTrim(Str(nRecno))
			EndIf
			cSufixo	+= ','+LTrim(Str(nOpc))

			cComando	+= '('+cSufixo+')'
		EndIf
		uRet	:= &(cComando)
	EndIf

Return uRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| Auditoria                       | Autor | Cirilo Rocha       | Data | 13/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Mostra auditoria do registro                                                      |##
//##|        | Feito de forma segregada porque o padr„o n„o consegui filtrar a consulta direta-  |##
//##|        |  mente, o alias fica setado na Z05 e n„o na tabela do browse                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function Auditoria()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local oArea				AS Object

	Private cFiltroZ05		AS Character

	//Inicializa Variaveis-------------------------------------------------------------------------
	cFiltroZ05	:= (cAlias)->(F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
	
	oArea		:= tCtrlAlias():GetArea({'SM0'})
	If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
		cFilAnt	:= (cAlias)->F1_FILIAL
		FWSM0Util():setSM0PositionBycFilAnt()
	EndIf

	ConPad1(,,,'Z05',,,.F.)

	oArea:RestArea()		//Restaura area
	oArea:Destroy()
	FWFreeVar(oArea)

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| ObsSC7                          | Autor | Cirilo Rocha       | Data | 14/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Visualiza observaÁıes do pedido de compras SC7                                    |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static nOP_OBSSC		:= 01		AS Numeric
Static nOP_OBSCAB		:= 02		AS Numeric
//---------------------------------------------------------------------------------------------
Static Function ObsSC7(	nOper	,;	//01 nOper
						lTodos	)	//02 lTodos

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aItensPC				AS Array
	Local cCampo				AS Character
	Local cChvDoc				AS Character
	Local cChvSc7				AS Character
	Local cObs					AS Character
	Local cMmObs				AS Character
	Local nX					AS Numeric
	Local oArea         		AS Object

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nOper		AS Numeric
	ParamType 1		VAR lTodos		AS Logical
	
	If Empty((cAlias)->D1_PEDIDO)
		U_CXMsgErro('Este documento N√O est· vinculado a um Pedido de Compras (D1_PEDIDO).',;
					/*cSolucao*/,/*lMsg*/,/*oArea*/)
		Return
	EndIf

	//---------------------------------------------------------------------------------------------
	aItensPC	:= {}
	If lTodos
		oArea		:= tCtrlAlias():GetArea({cAlias})
		cChvDoc		:= (cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
		(cAlias)->(U_CXSetOrd(1))
		(cAlias)->(dbSeek(cChvDoc))
		While 	(cAlias)->(!EOF()) .And. ;
				(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChvDoc
			
			cChvSc7	:= FwXfilial('SC7',(cAlias)->F1_FILIAL)+(cAlias)->(D1_PEDIDO+D1_ITEMPC)
			If 	aScan(aItensPC,{|x| x == cChvSc7 }) == 0
				aAdd(aItensPC,cChvSc7)
			EndIf

			(cAlias)->(dbSkip())
		EndDo
		oArea:RestArea()		//Restaura area
		oArea:Destroy()
		FWFreeVar(oArea)
	Else
		If FWIsInCallStack('TELACONFERENCIA')	//Veio da tela de conferÍncia
			aAdd(aItensPC,	FwXfilial('SC7',(cAlias)->F1_FILIAL)+;
							oModelCache:GetValue('MODEL_SD1','D1_PEDIDO')+;
							oModelCache:GetValue('MODEL_SD1','D1_ITEMPC'))
		Else
			aAdd(aItensPC,FwXfilial('SC7',(cAlias)->F1_FILIAL)+(cAlias)->(D1_PEDIDO+D1_ITEMPC))
		EndIf
	EndIf

	//---------------------------------------------------------------------------------------------
	If nOper == nOP_OBSSC
		cCampo	:= 'C7_OBSMEMO'
	ElseIf nOper == nOP_OBSCAB
		cCampo	:= 'C7_OBSCABE'
	EndIf

	cMmObs	:= ''
	SC7->(dbSetOrder(1))
	For nX := 1 to Len(aItensPC)
		If .Not. Empty(aItensPC[nX])
			SC7->(dbSeek(aItensPC[nX]))
			cObs	:= AllTrim(U_CXFieldGet(cCampo,'SC7'))
			If .Not. Empty(cObs)
				cMmObs	+= RTrim(SC7->C7_PRODUTO) + " - "+cObs
				If nX <> Len(aItensPC)
					cMmObs	+= CRLF
				EndIf
			EndIf
		EndIf
	Next

	If Empty(cMmObs)
		ApMsgInfo(	'Nenhuma observaÁ„o.',U_CXTxtMsg(,,.T.))
	Else
		u_CXApMsgMemo(cMmObs,U_CXTxtMsg()+"ObservaÁıes",.F.)
	EndIf

Return

//#############################################################################
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Programa  | ModelDef | Autor | Cirilo Rocha       | Data | 25/10/2022  |##
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Descr.    | FunÁ„o para montar as estrutura de dados e as regras       |##
//##+----------+----------+-------------------------------------------------+##
//##| DATA     | ANALISTA | MANUTEN«√O EFETUADA                             |##
//##+----------+----------+-------------------------------------------------+##
//##|          |          |                                                 |##
//##|          |          |                                                 |##
//##|          |          |                                                 |##
//##+----------+----------+-------------------------------------------------+##
//#############################################################################
Static Function ModelDef();
						AS Object

	//Declaracao de variaveis----------------------------------------------------------------------
	Local oModel    AS Object
	Local oGrid		AS Object

	Local oStruSF1  AS Object
	Local oStruSA2  AS Object
	Local oStruSD1  AS Object
	Local oStruSFT  AS Object

	// ConstruÁ„o de uma estrutura de dados--------------------------------------------------------
	oStruSF1 := FwFormStruct( nTP_MODEL, 'SF1' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ ) 
	oStruSA2 := FwFormStruct( nTP_MODEL, 'SA2' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )
	oStruSD1 := FwFormStruct( nTP_MODEL, 'SD1' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )
	oStruSFT := FwFormStruct( nTP_MODEL, 'SFT' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )

	//Ajusto manualmente o tamanho destes campos para exibiÁ„o
	oStruSF1:SetProperty('F1_YUSRLAN' 	, MVC_MODEL_TAMANHO,35)
	oStruSF1:SetProperty('F1_YUSRCFG' 	, MVC_MODEL_TAMANHO,35)
	oStruSD1:SetProperty('D1_COD'	 	, MVC_MODEL_TAMANHO,35)

	//Desmarco os campos obrigatÛrios, È somente visualizaÁ„o N√O precisa
	oStruSF1:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)
	oStruSA2:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)
	oStruSFT:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)

	oStruSF1:AddField(	'Natureza Financeira',;	//[01] C Titulo do campo (X3Titulo())
						'Natureza Financeira',;	//[02] C ToolTip do campo (X3Descric())
						'F1_NATUREZ'		,;	//[03] C identificador (ID) do Field (X3_CAMPO)
						'C'					,;	//[04] C Tipo do campo (X3_TIPO)
						35					,;	//[05] N Tamanho do campo (X3_TAMANHO)
						0					,;	//[06] N Decimal do campo (X3_DECIMAL)
						/*bValid*/			,;	//[07] B Code-block de validaÁ„o do campo (B.E. X3_VALID)
						/*bWhen*/			,;	//[08] B Code-block de validaÁ„o When do campo (B.E. X3_WHEN)
						/*aValues*/			,;	//[09] A Lista de valores permitido do campo (X3CBox())
						/*lObrigat*/		,;	//[10] L Indica se o campo tem preenchimento obrigatÛrio (X3Obrigat())
						/*bInit*/			,;	//[11] B Code-block de inicializacao do campo (B.E. X3_RELACAO)
						.F.					,;	//[12] L Indica se trata de um campo chave
						/*lNoUpd*/			,;	//[13] L Indica se o campo pode receber valor em uma operaÁ„o de update.
						.T.					,;	//[14] L Indica se o campo È virtual (X3_CONTEXT=V)
						/*cValid*/			)	//[15] C Valid do usu·rio em formato texto e sem alteraÁ„o, usado para se criar o aHeader de compatibilidade

	oModel   := MpFormModel():New(	'RFISA03M'		,;	//01 cID	//N„o pode ter o mesmo nome do fonte
									/*bPreVld*/		,; 	//02 bPre
									/*bPostVld*/	,;	//03 bPostVld
									/*bSave*/		,;	//04 bSave
									/*bCancel*/		)	//05 bCancel

	//Cabecalho (SF1)------------------------------------------------------------------------------
	oModel:AddFields(	'MODEL_SF1'	,;	//01 cID
						/*cOwner*/	,;	//02 cOwner
						oStruSF1	,;	//03 oModelStruct
						/*bPre*/	,;	//04 bPre
						/*bPost*/	,;	//05 bPost
						{|oFieldModel, lCopy| loadField(oFieldModel,lCopy) } ) 	//06 bLoad

	oModel:GetModel('MODEL_SF1'):SetPrimaryKey( { 'F1_FILIAL','F1_DOC','F1_SERIE','F1_FORNECE','F1_LOJA' } )

	// Adiciona a DescriÁ„o do Componente do Modelo de Dados
	oModel:SetDescription('ConferÍncia Fiscal')

	// Adiciona a descriÁ„o dos Componentes do Modelo de Dados
	oModel:GetModel( 'MODEL_SF1' ):SetDescription( 'CabeÁalho (SF1)' )

 	//Fornecedor/Cliente (SA1/SA2)-----------------------------------------------------------------
 	oModel:AddFields(	'MODEL_SA2'	,;	//01 cID
						'MODEL_SF1'	,;	//02 cOwner
						oStruSA2	,;	//03 oModelStruct
						/*bPre*/	,;	//04 bPre
						/*bPost*/	,;	//05 bPost
						{|oFieldModel, lCopy| loadField(oFieldModel,lCopy) } ) 	//06 bLoad

	//Itens do Documento (SD1)---------------------------------------------------------------------
	oModel:AddGrid(	'MODEL_SD1'		,;	//01 cID Modelo
	 				'MODEL_SF1'		,;	//02 cIDOwner
	 				oStruSD1 		,;	//03 oModelStruct
	 				/*bLinePre*/	,;	//04 bLinePre
	 				/*bLinhaOK*/	,;	//05 ValidaÁ„o equivale ao LinhaOK
	 				/*bPre*/		,;	//06 bPre
	 				/*bPost*/ 		,;	//07 bPost
	 				 {|oFieldModel, lCopy| loadField(oFieldModel,lCopy,.T.) } ) 	//06 bLoad

	oGrid	:= oModel:GetModel( 'MODEL_SD1' )
	oGrid:SetOptional( .T. )
	oGrid:SetUniqueLine( { 'D1_ITEM' } )
//	oGrid:SetMaxLine(99)
	oGrid:SetDescription( 'Itens Documento (SD1)' )

 	//Livros Fiscais (SFT)---------------------------------------------------------------------------
 	oModel:AddFields(	'MODEL_SFT'	,;	//01 cID
						'MODEL_SD1'	,;	//02 cOwner
						oStruSFT	,;	//03 oModelStruct
						/*bPre*/	,;	//04 bPre
						/*bPost*/	,;	//05 bPost
						{|oFieldModel, lCopy| loadField(oFieldModel,lCopy) } ) 	//06 bLoad
	
	//FT_FILIAL+FT_TIPOMOV+FT_SERIE+FT_NFISCAL+FT_CLIEFOR+FT_LOJA+FT_ITEM+FT_PRODUTO
	//oModel:SetRelation('MODEL_SFT', {;	//N√O … NECESS¡RIO PORQUE J¡ ESTOU USANDO UM BLOCO DE LEITURA CUSTOMIZADO
	//									{"FwXFilial('SFT')",'F1_FILIAL'},;
	//									{'FT_TIPOMOV',"'E'"},;
	//									{'FT_SERIE','F1_SERIE'},;
	//									{'FT_NFISCAL','F1_DOC'},;
	//									{'FT_CLIEFOR','F1_FORNECE'},;
	//									{'FT_LOJA','F1_LOJA'},;
	//									{'FT_ITEM','D1_ITEM'};
	//								}, SFT->(IndexKey(1)))

//	oModel:AddGrid(	'MODEL_SFT'		,;	//01 cID Modelo
//	 				'MODEL_SD1'		,;	//02 cIDOwner
//	 				oStruSFT 		,;	//03 oModelStruct
//	 				/*bLinePre*/	,;	//04 bLinePre
//	 				/*bLinhaOK*/	,;	//05 ValidaÁ„o equivale ao LinhaOK
//	 				/*bPre*/		,;	//06 bPre
//	 				/*bPost*/ 		,;	//07 bPost
//	 				{|oFieldModel, lCopy| loadField(oFieldModel,lCopy,.T.) } ) 	//06 bLoad

Return( oModel )

//#############################################################################
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Programa  | ViewDef  | Autor | Cirilo Rocha       | Data | 25/10/2022  |##
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Descr.    | FunÁ„o para montar a interface da tela                     |##
//##+----------+----------+-------------------------------------------------+##
//##| DATA     | ANALISTA | MANUTEN«√O EFETUADA                             |##
//##+----------+----------+-------------------------------------------------+##
//##|          |          |                                                 |##
//##|          |          |                                                 |##
//##|          |          |                                                 |##
//##+----------+----------+-------------------------------------------------+##
//#############################################################################
Static Function ViewDef();
					AS Object

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nX			AS Numeric
	Local oModel    	AS Object
	Local oView    		AS Object

	Local oStruSF1		AS Object
	Local oStruSA2		AS Object
	Local oStruSD1		AS Object
	Local oStruSFT		AS Object

	//InicializaÁ„o de Vari·veis-------------------------------------------------------------------
	oModel    	:= FWLoadModel('RFISA03')	//Nome do fonte onde est· o modelo

	oView		:= FWFormView():New()

	oView:SetModel( oModel )
//	oView:SetContinuousForm()

	oView:addUserButton('ObservaÁıes (F3)'		,;	//01 cTitle
						'CLIPS'					,;	//02 cResource
						{|| TelaObs()}			,;	//03 bBloco
						'ObservaÁıes (F3)'		,;	//04 cToolTip
						/*VK_F3*/				,;	//05 nShortCut
						{MODEL_OPERATION_VIEW}	,;	//06 aOptions
						.F.						)	//07 lShowBar

	//Adiciono botıes customizados na view---------------------------------------------------------
	For nX := 1 to Len(aXRotina)
		If aXRotina[nX][nRX_VIEW]
			oView:addUserButton(aXRotina[nX][cRX_DESC]	,;	//01 cTitle
								'CLIPS'					,;	//02 cResource
								aXRotina[nX][bRX_EXEC]	,;	//03 bBloco
								aXRotina[nX][cRX_DESC]	,;	//04 cToolTip
								/*aXRotina[nX][nRX_KEY]*/,;	//05 nShortCut
								{MODEL_OPERATION_VIEW}	,;	//06 aOptions
								.T.						)	//07 lShowBar
		EndIf
	Next
	
	oView:addUserButton('<< Doc. Anterior (F11)',;	//01 cTitle
						'CLIPS'					,;	//02 cResource
						{|| DocAnterior()}		,;	//03 bBloco
						'<< Doc. Anterior (F11)',;	//04 cToolTip
						/*VK_F11*/				,;	//05 nShortCut
						{MODEL_OPERATION_VIEW}	,;	//06 aOptions
						.T.						)	//07 lShowBar

	oView:addUserButton('PrÛximo Doc. (F12) >>'	,;	//01 cTitle
						'CLIPS'					,;	//02 cResource
						{|| ProximoDoc()}		,;	//03 bBloco
						'PrÛximo Doc. (F12) >>'	,;	//04 cToolTip
						/*VK_F12*/				,;	//05 nShortCut
						{MODEL_OPERATION_VIEW}	,;	//06 aOptions
						.T.						)	//07 lShowBar

	// ConstruÁ„o de uma estrutura de dados
	oStruSF1 := FwFormStruct( nTP_VIEW, 'SF1' ,{|cCpo| FiltraCpo(cCpo) }) 
	oStruSA2 := FwFormStruct( nTP_VIEW, 'SA2' ,{|cCpo| FiltraCpo(cCpo) })
	oStruSD1 := FwFormStruct( nTP_VIEW, 'SD1' ,{|cCpo| FiltraCpo(cCpo) })
	oStruSFT := FwFormStruct( nTP_VIEW, 'SFT' ,{|cCpo| FiltraCpo(cCpo) })

	//Coloca as descriÁıes completas nos campos
	U_CXDescMVC(oStruSF1,.T.)
	U_CXDescMVC(oStruSA2,.T.)
	U_CXDescMVC(oStruSD1,.F.)
	U_CXDescMVC(oStruSFT,.F.)

	//Para ganhar um pouco de espaÁo, estou colocando no nome do fornecedor
	oStruSF1:RemoveField('F1_FORNECE')	//Remove campo da estrutura
	oStruSF1:RemoveField('F1_LOJA')		//Remove campo da estrutura

	//Monta o combo do tipo de NF, no padr„o n„o existe combo!
	oStruSF1:SetProperty('F1_TIPO' , MVC_VIEW_COMBOBOX,U_CXTipoNF(4))
	//Uso a descriÁ„o resumida mesmo, a completa È muito ruim
	oStruSF1:SetProperty('F1_TIPO' , MVC_VIEW_TITULO,FwX3Titulo('F1_TIPO'))
	
	oStruSF1:AddField(	'F1_NATUREZ'		,;  //[01] C Nome do Campo (X3_CAMPO)
						'07'        		,;  //[02] C Ordem (X3_ORDEM)
						'Natureza Financeira',;  //[03] C Titulo do campo (X3Titulo())
						'Natureza Financeira',;  //[04] C DescriÁ„o do campo (X3Descric())
						/*aHelp*/			,;  //[05] A Array com Help
						'GET'		   		,;  //[06] C Tipo do campo (**)
						'@!'        		,;  //[07] C Picture (X3_PICTURE)
						/*bPicVar*/ 		,;  //[08] B Bloco de Picture Var (B.E. X3_PICTVAR)
						/*cF3*/        		,;  //[09] C Consulta F3 (X3_F3)
						.F.         		,;  //[10] L Indica se o campo È edit·vel (X3_VISUAL<>V)
						/*cFolder*/    		,;  //[11] C Pasta do campo (X3_FOLDER)
						/*cAgrupa*/    		,;  //[12] C Agrupamento do campo (X3_AGRUP)
						/*aCombo*/  		,;  //[13] A Lista de valores permitido do campo (X3_COMBO) 
						/*nMaxComb*/  		,;  //[14] N Tamanho Maximo da maior opÁ„o do combo
						/*cInicBrw*/  		,;  //[15] C Inicializador de Browse (X3_INIBRW)
						.T.         		,;  //[16] L Indica se o campo È virtual (X3_CONTEXT=V)
						/*cPictVar*/   		)   //[17] C Picture Vari·vel

	oStruSFT:SetProperty('FT_BASEPIS', MVC_VIEW_TITULO,'Base de C·lculo Pis/Cofins')
	oStruSFT:SetProperty('FT_CSTPIS', MVC_VIEW_TITULO,'Cod.Sit.Trib. Pis/Cofins')
	
	oStruSD1:SetProperty('D1_TES'	, MVC_VIEW_TITULO,'TES')
	oStruSD1:SetProperty('D1_CF'	, MVC_VIEW_TITULO,'CFOP')
	oStruSD1:SetProperty('D1_ITEM'	, MVC_VIEW_TITULO,'Item')
	oStruSD1:SetProperty('D1_PEDIDO', MVC_VIEW_TITULO,'Pedido')
	oStruSD1:SetProperty('D1_ITEMPC', MVC_VIEW_TITULO,'It PC')

	//Para n„o ocorrer erros na abertura da tela com a funÁ„o padr„o!
	oStruSA2:SetProperty('A2_CGC' , MVC_VIEW_PVAR,FwBuildFeature(STRUCT_FEATURE_PICTVAR , 'U_CXFJ()'))
	//Retiro os folders
	oStruSA2:SetProperty('*', MVC_VIEW_FOLDER_NUMBER,"")
	oStruSA2:aFolders	:= {}

	oStruSA2:SetProperty('A2_NOME'	, MVC_VIEW_TITULO,'CÛdigo-Loja-Nome ou Raz„o Social Fornecedor/Cliente')
	oStruSA2:SetProperty('A2_CGC'	, MVC_VIEW_TITULO,'CNPJ/CPF do Fornecedor/Cliente')

	oView:AddField( 'VIEW_SF1'	, oStruSF1	, 'MODEL_SF1' )	//CabeÁalho docto (SF1)
	oView:AddField( 'VIEW_SA2'	, oStruSA2	, 'MODEL_SA2' )	//Fornecedor/Cliente (SA2/SA1)
	oView:AddGrid(	'VIEW_SD1'	, oStruSD1	, 'MODEL_SD1' )	//Itens Docto (SD1)
	//oView:AddGrid(	'VIEW_SFT'	, oStruSFT	, 'MODEL_SFT' )	//Livros Fiscais (SFT)
	oView:AddField(	'VIEW_SFT'	, oStruSFT	, 'MODEL_SFT' )	//Livros Fiscais (SFT)


	//Painel Superior------------------------------------------------------------------------------
	oView:CreateVerticalBox(  'PAINEL_PRINCIPAL', 100 )

		oView:EnableTitleView('VIEW_SF1','CabeÁalho (SF1)')
		oView:CreateHorizontalBox( 'CABECALHO' ,  40, 'PAINEL_PRINCIPAL' )
		oView:SetOwnerView( 'VIEW_SF1' 	, 'CABECALHO')	//Relaciona o identificador (ID) da View com o 'box' para exibiÁ„o
		
		oView:EnableTitleView('VIEW_SA2','Fornecedor (SA2) / Cliente (SA1)')
		oView:CreateHorizontalBox( 'FOLDER_SA2', 15, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SA2' 	, 'FOLDER_SA2' )	//Relaciona o identificador (ID) da View com o 'box' para exibiÁ„o
		
		oView:EnableTitleView('VIEW_SD1',oModel:GetModel( 'MODEL_SD1' ):GetDescription())
		oView:CreateHorizontalBox( 'GRID_SD1', 25, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SD1' 	, 'GRID_SD1' )	//Relaciona o identificador (ID) da View com o 'box' para exibiÁ„o

		oView:EnableTitleView('VIEW_SFT','Livros Fiscais (SFT)')
		oView:CreateHorizontalBox( 'FOLDER_SFT', 20, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SFT' 	, 'FOLDER_SFT' )	//Relaciona o identificador (ID) da View com o 'box' para exibiÁ„o

	//Fim PAINEL_PRINCIPAL

Return( oView )

//-------------------------------------------------------------------------------------------------
Static Function FiltraCpo(cCpo)

Return aScan(aCampos,{|x| x == RTrim(cCpo)}) > 0

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| loadField                       | Autor | Cirilo Rocha       | Data | 22/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | FunÁ„o respons·vel pela carga dos dados para o modelo MVC                         |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function loadField(	oFieldModel	,;	//01 oFieldModel
							lCopy		,;	//02 lCopy
							lGrid		);	//03 lGrid
								AS Array

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aLoad := {}	AS Array
	Local aFields		AS Array
	Local aDados		AS Array
	Local cChvDoc		AS Character
	Local oArea         AS Object

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR oFieldModel		AS Object
	ParamType 1		VAR lCopy			AS Logical
	ParamType 2		VAR lGrid			AS Logical		Optional Default .F.

	//---------------------------------------------------------------------------------------------
	aFields	:= @oFieldModel:oFormModelStruct:aFields
	If lGrid

		oArea		:= tCtrlAlias():GetArea({cAlias})
		If FwTabPref(aFields[1][MODEL_FIELD_IDFIELD]) == 'SFT'	//Tabela Neta!
			(cAlias)->(dbGoTo(oModelCache:GetModel('MODEL_SD1'):GetDataId()))	//Posiciona a tabela
			
			aDados	:= CarregaDados(aFields)
			aAdd(aLoad, {(cAlias)->(Recno()),aDados} ) //dados
		Else
			cChvDoc		:= (cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
			(cAlias)->(dbSetOrder(1))
			(cAlias)->(dbSeek(cChvDoc))
			While 	(cAlias)->(!EOF()) .And. ;
					(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChvDoc
				
				aDados	:= CarregaDados(aFields)
				aAdd(aLoad, {(cAlias)->(Recno()),aDados} ) //dados
				(cAlias)->(dbSkip())
			EndDo
		EndIf
		oArea:RestArea()		//Restaura area
		oArea:Destroy()
		FWFreeVar(oArea)
	Else
		If FwTabPref(aFields[1][MODEL_FIELD_IDFIELD]) == 'SFT'	//Tabela Neta!
			(cAlias)->(dbGoTo(oModelCache:GetModel('MODEL_SD1'):GetDataId()))	//Posiciona a tabela
		EndIf

		aAdd(aLoad, CarregaDados(aFields) ) //dados
		aAdd(aLoad, 1) 						//recno
	EndIf

Return aLoad

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| CarregaDados                    | Autor | Cirilo Rocha       | Data | 22/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | FunÁ„o auxiliar para carregar os dados dos modelos MVC                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function CarregaDados(aFields);	//01 aFields
								AS Array
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local aDados	:= {}		AS Array
	Local nX					AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aFields		AS Array

	//---------------------------------------------------------------------------------------------
	For nX := 1 to Len(aFields)
		
		If aFields[nX][MODEL_FIELD_IDFIELD] == 'F1_NATUREZ'
			aAdd(aDados,RTrim(U_CXFieldGet('E2_NATUREZ',cAlias))+'-'+RTrim(U_CXFieldGet('ED_DESCRIC',cAlias)))
		Else
			aAdd(aDados,U_CXFieldGet(aFields[nX][MODEL_FIELD_IDFIELD],cAlias))
			//Para mostrar esse campo com o detalhamento
			If aFields[nX][MODEL_FIELD_IDFIELD] == 'F1_YUSRCFG'
				aTail(aDados)	:= U_RFISA03A(aTail(aDados))
			ElseIf aFields[nX][MODEL_FIELD_IDFIELD] == 'F1_YUSRLAN'
				aTail(aDados)	:= aTail(aDados)+'-'+IIF(lOfuscar,Replicate('*',35),RTrim(U_CXFieldGet('USR_NOME',cAlias)))
			ElseIf aFields[nX][MODEL_FIELD_IDFIELD] == 'D1_COD'
				aTail(aDados)	:= RTrim(aTail(aDados))+'-'+IIF(lOfuscar,Replicate('*',FWTamSX3('B1_DESC')[1]),RTrim(U_CXFieldGet('B1_DESC',cAlias)))
			ElseIf aFields[nX][MODEL_FIELD_IDFIELD] == 'A2_NOME'
				aTail(aDados)	:= U_CXFieldGet('F1_FORNECE',cAlias)+'-'+U_CXFieldGet('F1_LOJA',cAlias)+'-'+RTrim(aTail(aDados))
			EndIf
		EndIf
	Next

Return aDados

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| ProximoDoc                      | Autor | Cirilo Rocha       | Data | 23/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Passa para o proximo documento a ser conferido.                                   |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function ProximoDoc()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cChvDoc		AS Character

	//Inicializa Variaveis-------------------------------------------------------------------------
	cChvDoc		:= (cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
	(cAlias)->(dbSetOrder(1))
	While 	(cAlias)->(!EOF()) .And. ;
			(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChvDoc
		
		(cAlias)->(dbSkip())
	EndDo

	If (cAlias)->(EOF())
		U_CXHelp(,,'Chegou ao final da relaÁ„o de documentos. N√O È possÌvel avanÁar.',,/*cSolucao*/,.T.)
	Else
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		oModelCache:DeActivate()	//ForÁa a recarga dos dados
		oModelCache:Activate()
		AtualizaTelaMVC()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| DocAnterior                     | Autor | Cirilo Rocha       | Data | 23/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Volta para o documento anterior                                                   |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function DocAnterior()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cChvDoc		AS Character

	//Inicializa Variaveis-------------------------------------------------------------------------
	cChvDoc		:= (cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
	(cAlias)->(dbSetOrder(1))
	While 	(cAlias)->(!BOF()) .And. ;
			(cAlias)->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA) == cChvDoc
		
		(cAlias)->(dbSkip(-1))
	EndDo

	If (cAlias)->(BOF())
		U_CXHelp(,,'Chegou ao inÌcio da relaÁ„o de documentos. N√O È possÌvel voltar mais.',,/*cSolucao*/,.T.)
	Else
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		oModelCache:DeActivate()	//ForÁa a recarga dos dados
		oModelCache:Activate()
		AtualizaTelaMVC()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| TelaObs                         | Autor | Cirilo Rocha       | Data | 24/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Tela auxiliar para conseguir mostrar as observaÁıes dentro da tela MVC, porque nos|##
//##|        |  botıes do MVC n„o existe suporte a sub-menus.                                    |##
//##|        | http://devforum.totvs.com.br/3405-adicionar-submenu-view-mvc                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function TelaObs()
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local aPosBt				AS Array
	Local nX					AS Numeric
	Local nAltTela				AS Numeric
	Local nAltBt	:= 020		AS Numeric	/*Integer*/
	Local nLarBt	:= 080		AS Numeric	/*Integer*/
	Local oDlg					AS Object		// Dialog Principal

	//---------------------------------------------------------------------------------------------
	//Controle para nao abrir varias vezes esta tela
	If lTelaObs //Verifica se a funcao passada esta dentro da pilha de execucao (s/ U_)
		Return
	EndIf

	lTelaObs	:= .T.
	nAltTela	:= ((Len(aMenuObs)+1)*(nAltBt+035))+035
	
	oDlg	:= MSDialog():New(	000,000,nAltTela,(nLarBt*3)+010,U_CXTxtMsg()+"ObservaÁıes PC/SC",;
								/*cPar6*/,/*nPar7*/,/*lPar8*/,/*DS_MODALFRAME*/,/*anClrText*/,/*anClrBack*/,;
								/*oPar12*/,/*oWnd*/,.T.,,,,/*lTransparent*/)
		
		aPosBt	:= U_CXPosBtn(oDlg,nLarBt,nAltBt)

		For nX := 1 to Len(aMenuObs)
			TButton():New( 	005+(nX-1)*(nAltBt+005), 025, aMenuObs[nX][cRX_DESC], oDlg,aMenuObs[nX][bRX_EXEC],;
							nLarBt, nAltBt,,,.F.,.T.,.F.,,.F.,,,.F. )
		Next
		 
		TButton():New( aPosBt[1], 025, "&Fechar", oDlg,{||  oDlg:end()}, nLarBt, nAltBt,,,.F.,.T.,.F.,,.F.,,,.F. )

	// ativa di·logo centralizado
	oDlg:Activate(/*uPar1*/,/*uPar2*/,/*uPar3*/,.T./*lCenter*/,/*{|Self| Valid }*/,/*uPar6*/,/*{|Self| Init }*/ )

	lTelaObs	:= .F.

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| AtualizaTelaMVC                 | Autor | Cirilo Rocha       | Data | 27/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | ForÁa a atualizaÁ„o do objeto MVC, quando estava chamando diretamente pelo atalho |##
//##|        |  F# o sistema n„o estava atualizando.                                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function AtualizaTelaMVC()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local oView	:= FWViewActive()			AS Object

	If ValType(oView) == 'O'
		oView:Refresh()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| Conhecimento                    | Autor | Cirilo Rocha       | Data | 16/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Chama a tela padr„o de Conhecimento (MPDocument)                                  |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTEN«√O EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
Static Function Conhecimento()

	//Declaracao de variaveis----------------------------------------------------------------------
	Private INCLUI	:= .F.		AS Logical
	Private ALTERA	:= .F.		AS Logical
	Private aRotina	:= {}		AS Array

	//---------------------------------------------------------------------------------------------
	SF1->(dbGoTo((cAlias)->F1_RECNO))
	aAdd(aRotina,{,,,nOPC_VISUAL})	//Para forÁar ficar somente visualizaÁ„o!

	MPDocument('SF1',SF1->(Recno()),1)

Return

///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
Static Function XMLNFEDemo()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cXMLNFEDemo	:= ''		AS Character

begincontent var cXMLNFEDemo
<nfeProc versao="4.00"
         xmlns="http://www.portalfiscal.inf.br/nfe">
	<NFe xmlns="http://www.portalfiscal.inf.br/nfe">
		<infNFe versao="4.00"
		        Id="NFe35080599999090910270550010000000015180051273">
			<ide>
				<cUF>23</cUF>
				<cNF>518005127</cNF>
				<natOp>VENDA MERC.ADQUIRIDA/RECEB.S.T.INTERRESTADUAL</natOp>
				<mod>55</mod>
				<serie>0</serie>
				<nNF>1042</nNF>
				<dhEmi>2023-03-13T10:34:23-03:00</dhEmi>
				<dhSaiEnt>2023-03-13T10:34:23-03:00</dhSaiEnt>
				<tpNF>1</tpNF>
				<idDest>2</idDest>
				<cMunFG>3550308</cMunFG>
				<tpImp>1</tpImp>
				<tpEmis>1</tpEmis>
				<cDV>4</cDV>
				<tpAmb>1</tpAmb>
				<finNFe>1</finNFe>
				<indFinal>1</indFinal>
				<indPres>1</indPres>
				<indIntermed>0</indIntermed>
				<procEmi>0</procEmi>
				<verProc>400</verProc>
			</ide>
			<emit>
				<CNPJ>99999090910270</CNPJ>
				<xNome>NF-e Associacao NF-e</xNome>
				<enderEmit>
					<xLgr>AV TRISTAO GONCALVES</xLgr>
					<nro>100</nro>
					<xBairro>CENTRO</xBairro>
					<cMun>2304400</cMun>
					<xMun>FORTALEZA</xMun>
					<UF>CE</UF>
					<CEP>60015002</CEP>
					<cPais>1058</cPais>
					<xPais>BRASIL</xPais>
					<fone>8532266869</fone>
				</enderEmit>
				<IE>066868491</IE>
				<IM>0</IM>
				<CRT>1</CRT>
			</emit>
			<dest>
				<CNPJ>00000000000000</CNPJ>
				<xNome>DISTRIBUIDORA DE AGUAS MINERAIS</xNome>
				<enderDest>
					<xLgr>AV DAS FONTES</xLgr>
					<nro>123</nro>
					<xCpl>0</xCpl>
					<xBairro>PARQUE FONTES</xBairro>
					<cMun>5030801</cMun>
					<xMun>SAO PAULO</xMun>
					<UF>SP</UF>
					<CEP>13950000</CEP>
					<cPais>1058</cPais>
					<xPais>BRASIL</xPais>
				</enderDest>
				<indIEDest>1</indIEDest>
				<IE>123484332</IE>
			</dest>
			<det nItem="1">
				<prod>
					<cProd>000022</cProd>
					<cEAN>SEM GTIN</cEAN>
					<xProd>FIO 22 AWG PASQUA</xProd>
					<NCM>85441100</NCM>
					<CFOP>6403</CFOP>
					<uCom>KG</uCom>
					<qCom>4</qCom>
					<vUnCom>76.35</vUnCom>
					<vProd>305.40</vProd>
					<cEANTrib>SEM GTIN</cEANTrib>
					<uTrib>KG</uTrib>
					<qTrib>4</qTrib>
					<vUnTrib>76.35</vUnTrib>
					<indTot>1</indTot>
				</prod>
				<imposto>
					<ICMS>
						<ICMSSN102>
							<orig>0</orig>
							<CSOSN>400</CSOSN>
						</ICMSSN102>
					</ICMS>
					<PIS>
						<PISOutr>
							<CST>49</CST>
							<vBC>0.00</vBC>
							<pPIS>0.0000</pPIS>
							<vPIS>0.00</vPIS>
						</PISOutr>
					</PIS>
					<COFINS>
						<COFINSOutr>
							<CST>49</CST>
							<vBC>0.00</vBC>
							<pCOFINS>0.0000</pCOFINS>
							<vCOFINS>0.00</vCOFINS>
						</COFINSOutr>
					</COFINS>
				</imposto>
			</det>
			<total>
				<ICMSTot>
					<vBC>0.00</vBC>
					<vICMS>0.00</vICMS>
					<vICMSDeson>0.00</vICMSDeson>
					<vFCP>0.00</vFCP>
					<vBCST>0.00</vBCST>
					<vST>0.00</vST>
					<vFCPST>0.00</vFCPST>
					<vFCPSTRet>0.00</vFCPSTRet>
					<vProd>305.40</vProd>
					<vFrete>0.00</vFrete>
					<vSeg>0.00</vSeg>
					<vDesc>0.00</vDesc>
					<vII>0.00</vII>
					<vIPI>0.00</vIPI>
					<vIPIDevol>0.00</vIPIDevol>
					<vPIS>0.00</vPIS>
					<vCOFINS>0.00</vCOFINS>
					<vOutro>0.00</vOutro>
					<vNF>305.40</vNF>
				</ICMSTot>
			</total>
			<transp>
				<modFrete>4</modFrete>
			</transp>
			<cobr>
				<fat>
					<nFat>001042</nFat>
					<vOrig>305.40</vOrig>
					<vDesc>0.00</vDesc>
					<vLiq>305.40</vLiq>
				</fat>
				<dup>
					<nDup>001</nDup>
					<dVenc>2023-04-08</dVenc>
					<vDup>305.40</vDup>
				</dup>
			</cobr>
			<pag>
				<detPag>
					<indPag>1</indPag>
					<tPag>15</tPag>
					<vPag>305.40</vPag>
				</detPag>
			</pag>
			<infAdic/>
		</infNFe>
		<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">
			<SignedInfo>
				<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>
				<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>
				<Reference URI="#NFe35080599999090910270550010000000015180051273">
					<Transforms>
						<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>
						<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>
					</Transforms>
					<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>
					<DigestValue>AohW9RrMSJ1zvENnXul6v5/Abj8=</DigestValue>
				</Reference>
			</SignedInfo>
			<SignatureValue>SozmyD+vRzyRy4IT+D0JjRXTaRU7XIj/+vY9CBJY+NvRYP9chZMAnOVUmkmjtBQnNSAFVI5Vq7dhRCcpUEptouHrYr44tJsBEQMfYtvjuz/TtfU1ZewN7VH1Hx7zamH2BR0KRn2Hv4OZ1S1LqFZOTvjr0z378Gj0RYxMGUJtbOIcOoe0RL19TJnTjnGL/zPsmojZnTcHZgCKDyTFoRMgB2cpteMnHagfetJdkHePBepGMqIC1xKxWc9jOCwJ9SvNFfYUlTG6ZK0SEArbE0s9748JjehB1pW43AGf0teHeND0z4P6EFQHbdhx1gwPjqTDBw3Oiuwbl6JG0b11HA6hMw==</SignatureValue>
			<KeyInfo>
				<X509Data>
					<X509Certificate>MIIHVDCCBTygAwIBAgIIcx4iBCU+qhowDQYJKoZIhvcNAQELBQAwWTELMAkGA1UEBhMCQlIxEzARBgNVBAoTCklDUC1CcmFzaWwxFTATBgNVBAsTDEFDIFNPTFVUSSB2NTEeMBwGA1UEAxMVQUMgU09MVVRJIE11bHRpcGxhIHY1MB4XDTIyMDQyNTEzNDIwMFoXDTIzMDQyNTEzNDIwMFowgfsxCzAJBgNVBAYTAkJSMRMwEQYDVQQKEwpJQ1AtQnJhc2lsMQswCQYDVQQIEwJDRTESMBAGA1UEBxMJRm9ydGFsZXphMR4wHAYDVQQLExVBQyBTT0xVVEkgTXVsdGlwbGEgdjUxFzAVBgNVBAsTDjIwOTM3MTMwMDAwMTYyMRkwFwYDVQQLExBWaWRlb2NvbmZlcmVuY2lhMRowGAYDVQQLExFDZXJ0aWZpY2FkbyBQSiBBMTFGMEQGA1UEAxM9RUxFVFJPRklPUyBDT01FUkNJTyBERSBNQVRFUklBUyBFTEVUUklDT1MgTFREQTowNjAxNDUwNjAwMDEzNjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKFbUKPAl2FiB2EGBfWq9d2M+G841d8obmwlQiOEe4moD9uWmLdtnWoHPuuMIXterWIdQnqQC7MLHwRypAO9AfmvPwnRPGjQnWEOVRORCcGf2202FTGBIlKS6eK14zhxmLKv8bGJUDSW0kp4PQqUAw0xnpc7jgV1H3k0638SAH9zBvBI98xmsD7NaqyGgcfBVUc8/JcjJxWuTzcvBSnMxfjjm/neIWml9++/Y+LxMnlwxhBzU053KQWzqVYIoM2R7/4l7KfCXQNGS1wOXbGBGtVllIExitseOy2BeZ+08y+IHRRh7P/QMvqHyzAeEjaBpFUjR9hnFO0VNi27x8LVwscCAwEAAaOCAnswggJ3MAkGA1UdEwQCMAAwHwYDVR0jBBgwFoAUxVLtJYAJ35yCyJ9Hxt20XzHdubEwVAYIKwYBBQUHAQEESDBGMEQGCCsGAQUFBzAChjhodHRwOi8vY2NkLmFjc29sdXRpLmNvbS5ici9sY3IvYWMtc29sdXRpLW11bHRpcGxhLXY1LnA3YjCBtgYDVR0RBIGuMIGrgRhjb20uZWxldHJvZmlvc0BnbWFpbC5jb22gIQYFYEwBAwKgGBMWUkFJTVVORE8gQlJBR0EgUEVSRUlSQaAZBgVgTAEDA6AQEw4wNjAxNDUwNjAwMDEzNqA4BgVgTAEDBKAvEy0yOTA0MTk1NzE3OTk1NTgyMzE1MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDCgFwYFYEwBAwegDhMMMDAwMDAwMDAwMDAwMF0GA1UdIARWMFQwUgYGYEwBAgEmMEgwRgYIKwYBBQUHAgEWOmh0dHA6Ly9jY2QuYWNzb2x1dGkuY29tLmJyL2RvY3MvZHBjLWFjLXNvbHV0aS1tdWx0aXBsYS5wZGYwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMEMIGMBgNVHR8EgYQwgYEwPqA8oDqGOGh0dHA6Ly9jY2QuYWNzb2x1dGkuY29tLmJyL2xjci9hYy1zb2x1dGktbXVsdGlwbGEtdjUuY3JsMD+gPaA7hjlodHRwOi8vY2NkMi5hY3NvbHV0aS5jb20uYnIvbGNyL2FjLXNvbHV0aS1tdWx0aXBsYS12NS5jcmwwHQYDVR0OBBYEFIwjCpPxiBXPQVUXwyysgVgBxoieMA4GA1UdDwEB/wQEAwIF4DANBgkqhkiG9w0BAQsFAAOCAgEAOgSEh/F4Wv8FUVrie4Gruyx3rRmYhJPwYXUDBZy7DlvTbd72WA7bjuk26pTMIccJdHXIfXj2k1w+8vLLsRz8Yra4clYu1KSY97V0k7eOZKYDlQ9+/wFgsizNAy6eWahU+zL0aTBfI2gLOVcOjQOZBrCSBndXtadmO99ThNoMpd2/LlO7wfzD/5GYzrYpAuNWgbjcx1cluvnBwTUPAjx/wf6Com7PiF2fCg/5ALcmIOleohAiNV8Xw2dwH3rVV7dzuhNofdSVnz1j0tNfG7fbp+f7fTaS7UUcD3VLD9AECOhk3aTmcEYAlasIAsrfZE2G7MIfBIHlEWTtqy2L9LZ7sWFSfZ6Gftkl35bycW5k2usqtxkXgSuXATwfXxhIvjGY2zwXqF1fWTaa6qBA6nfoVeoXJdxEiM93dFBpk8aQqtwxN4zcQSOCjS6BZW8kK7givgPJCc7UippxGZQd4nYqMD3LcA4z/a+Sc+9lX+/hb4cEKv2nQImSPuhotahKvY9VOIEaKvf0xR3K5TvXg96cR7EhJInFoF4vBYVLqnAmwUTn+soARxpqVoDtx1fo9X8mUnsEu/BpXBZmJ0PpL7zmU3SgUHQs1elP+e6fjUj1q/Zuh9YloIB0Y34Y2lgMHGT22j7Ls+5qLFmZwnj72nTHBoVu7mOASbzX5Ek0EfXFmpA=</X509Certificate>
				</X509Data>
			</KeyInfo>
		</Signature>
	</NFe>
	<protNFe versao="4.00"
	         xmlns="http://www.portalfiscal.inf.br/nfe">
		<infProt>
			<tpAmb>1</tpAmb>
			<verAplic>SVRS202301190912</verAplic>
			<chNFe>35080599999090910270550010000000015180051273</chNFe>
			<dhRecbto>2023-03-13T10:34:32-03:00</dhRecbto>
			<nProt>323230017316101</nProt>
			<digVal>AohW9RrMSJ1zvENnXul6v5/Abj8=</digVal>
			<cStat>100</cStat>
			<xMotivo>Autorizado o uso da NF-e</xMotivo>
		</infProt>
	</protNFe>
</nfeProc>
endcontent

Return cXMLNFEDemo

Static Function XMLCTEDemo()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cXMLCTEDemo	:= ''		AS Character

begincontent var cXMLCTEDemo
<cteProc versao="3.00"
         xmlns="http://www.portalfiscal.inf.br/cte">
	<CTe>
		<infCte Id="CTe99999704012799900349570040000165560001225662"
		        versao="3.00">
			<ide>
				<cUF>51</cUF>
				<cCT>000122566</cCT>
				<CFOP>6932</CFOP>
				<natOp>Transporte de Carga</natOp>
				<mod>57</mod>
				<serie>1</serie>
				<nCT>907</nCT>
				<dhEmi>2023-03-18T00:00:00-03:00</dhEmi>
				<tpImp>1</tpImp>
				<tpEmis>1</tpEmis>
				<cDV>6</cDV>
				<tpAmb>1</tpAmb>
				<tpCTe>0</tpCTe>
				<procEmi>0</procEmi>
				<verProc>3</verProc>
				<cMunEnv>5103403</cMunEnv>
				<xMunEnv>Cuiaba</xMunEnv>
				<UFEnv>MT</UFEnv>
				<modal>01</modal>
				<tpServ>0</tpServ>
				<cMunIni>2408003</cMunIni>
				<xMunIni>Mossoro</xMunIni>
				<UFIni>RN</UFIni>
				<cMunFim>5106752</cMunFim>
				<xMunFim>Pontes e Lacerda</xMunFim>
				<UFFim>MT</UFFim>
				<retira>1</retira>
				<indIEToma>1</indIEToma>
				<toma3>
					<toma>3</toma>
				</toma3>
			</ide>
			<compl>
				<Entrega>
					<semData>
						<tpPer>0</tpPer>
					</semData>
					<semHora>
						<tpHor>0</tpHor>
					</semHora>
				</Entrega>
			</compl>
			<emit>
				<CNPJ>00000000000100</CNPJ>
				<IE>137973594</IE>
				<xNome>WebDANFE</xNome>
				<xFant>WebDANFE</xFant>
				<enderEmit>
					<xLgr>Rua Timao</xLgr>
					<nro>17</nro>
					<xCpl>EDIF SB TOWER SALA 811</xCpl>
					<xBairro>ALVORADA</xBairro>
					<cMun>3534401</cMun>
					<xMun>Sao Paulo</xMun>
					<CEP>08383015</CEP>
					<UF>SP</UF>
					<fone>01135556200</fone>
				</enderEmit>
			</emit>
			<rem>
				<CNPJ>99999999999199</CNPJ>
				<IE>42424242424</IE>
				<xNome>Willy Wonka Chocolates S.A</xNome>
				<xFant>Willy Wonka Chocolates S.A</xFant>
				<enderReme>
					<xLgr>R do Folclore</xLgr>
					<nro>928</nro>
					<xCpl>KM 30</xCpl>
					<xBairro>CENTRO</xBairro>
					<cMun>3522901</cMun>
					<xMun>Curupira</xMun>
					<CEP>00000042</CEP>
					<UF>SP</UF>
					<cPais>1058</cPais>
					<xPais>BRASIL</xPais>
				</enderReme>
			</rem>
			<dest>
				<CNPJ>8422428306823</CNPJ>
				<IE>321312412</IE>
				<xNome>Zombie Factory LTDA</xNome>
				<enderDest>
					<xLgr>Rua dos 4 olhos</xLgr>
					<nro>S/N</nro>
					<xCpl>KM 231</xCpl>
					<xBairro>INDUSTRIAL</xBairro>
					<cMun>3534401</cMun>
					<xMun>Zombieland</xMun>
					<CEP>43234225</CEP>
					<UF>MT</UF>
					<cPais>1058</cPais>
					<xPais>BRASIL</xPais>
				</enderDest>
			</dest>
			<vPrest>
				<vTPrest>14132.31</vTPrest>
				<vRec>14132.31</vRec>
			</vPrest>
			<imp>
				<ICMS>
					<ICMSSN>
						<CST>90</CST>
						<indSN>1</indSN>
					</ICMSSN>
				</ICMS>
			</imp>
			<infCTeNorm>
				<infCarga>
					<vCarga>4354.56</vCarga>
					<proPred>sal</proPred>
					<vCargaAverb>4354.56</vCargaAverb>
				</infCarga>
				<infDoc>
					<infNFe>
						<chave>99999999012744000349570040000165560001225662</chave>
					</infNFe>
				</infDoc>
				<infModal versaoModal="3.00">
					<rodo>
						<RNTRC>12145464</RNTRC>
					</rodo>
				</infModal>
			</infCTeNorm>
		</infCte>
		<infCTeSupl>
			<qrCodCTe>https://www.sefaz.mt.gov.br/cte/qrcode?chCTe=99999999012744000349570040000165560001225662&amp;tpAmb=1</qrCodCTe>
		</infCTeSupl>
		<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">
			<SignedInfo>
				<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>
				<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>
				<Reference URI="#CTe99999999012744000349570040000165560001225662">
					<Transforms>
						<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>
						<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>
					</Transforms>
					<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>
					<DigestValue>tDFH20nyPp2EqHInZHa/MXy+1Y0=</DigestValue>
				</Reference>
			</SignedInfo>
			<SignatureValue>hbxvbJuKnCVG7ym/BQFZvshzAcwPiJ3gBvtCnfo1upnPu2Lli/p8Q/03pjgMNhqDGxVSvGlwjBqSHms+nWonGpnk39TMoOV9KftVkBwFVRdNlY6QxrB/4JN8nS0OBpRD1bUH9XRmkC/d1ICjEa7gYsCiKG8VtKkmpv39JuMLBCEHRJ264OLNCOTnLzLHUxb5w3FqoH19ea6YLHZzte0XeEDat8ZHj1mufQnhjm8lbFYvATbu/FU5Y3Gm8IHTlq9Se6RWfdKSEmswIkaGJsXAUnBIJLN1npW11PhFgitdOJVtsg6Xjrf7KMz6MFXFakFPS7WlFcICJAzqhUvO/CkKHg==</SignatureValue>
			<KeyInfo>
				<X509Data>
					<X509Certificate>MIIHSTCCBTGgAwIBAgIIVQQjAQNmWgwwDQYJKoZIhvcNAQELBQAwWTELMAkGA1UEBhMCQlIxEzARBgNVBAoTCklDUC1CcmFzaWwxFTATBgNVBAsTDEFDIFNPTFVUSSB2NTEeMBwGA1UEAxMVQUMgQ0VSVElGSUNBIE1JTkFTIHY1MB4XDTIzMDEwNDE5MzYwMFoXDTI0MDEwNDE5MzYwMFowgeMxCzAJBgNVBAYTAkJSMRMwEQYDVQQKEwpJQ1AtQnJhc2lsMQswCQYDVQQIEwJNVDEVMBMGA1UEBxMMUm9uZG9ub3BvbGlzMR4wHAYDVQQLExVBQyBDRVJUSUZJQ0EgTUlOQVMgdjUxFzAVBgNVBAsTDjEwNTI4MTExMDAwMTI5MRkwFwYDVQQLExBWaWRlb2NvbmZlcmVuY2lhMRowGAYDVQQLExFDZXJ0aWZpY2FkbyBQSiBBMTErMCkGA1UEAxMiTUcgVFJBTlNQT1JURVMgTFREQTowODgxNDQyMjAwMDEwMjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAIeh09j9jD8AgFQmR1U60gH7ay0J+oyaCF8ibX2+dRBkN+IjL+ppUTpqYH9xkaf7k7vgzcfJuzaE4l2Eq0fJx6A396iLCSGrqPi+dgy6tFBWUs9eGKDMD+3F8w14/UeLvF18pyzVY8lxD/o9KczlS+iLprhauOBNbQdAmuAT8Xwa+imHYvebfJbd1HY8KfPyXZj3FeMrUhwqlf1I9aIF/vwK9HWAcjMDK4V60f396MAA/L1mT9/g1+OFZy93l3ypmEfWepLV5WHRQ2OHkUW6i5xozqnXEH1s0wVsuPqBhyCDaTze4dxaBBoIRFDO1P+TkfI89s1fqnwmWiHe5UiYkFUCAwEAAaOCAogwggKEMB8GA1UdIwQYMBaAFD/TXKkZTdeIFi2YDK8K3uFPJBawMFkGCCsGAQUFBwEBBE0wSzBJBggrBgEFBQcwAoY9aHR0cDovL2NjZC5hY3NvbHV0aS5jb20uYnIvbGNyL2FjLWNlcnRpZmljYW1pbmFzLXNtaW1lLXY1LnA3YjCBugYDVR0RBIGyMIGvgRhjbGVpbHNvbi5ndWltYUBnbWFpbC5jb22gJQYFYEwBAwKgHBMaQ0xFSUxTT04gTUVORVpFUyBHVUlNQVJBRVOgGQYFYEwBAwOgEBMOMDg4MTQ0MjIwMDAxMDKgOAYFYEwBAwSgLxMtMTUwOTE5NzY2MjEyNDA0ODEwNDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwoBcGBWBMAQMHoA4TDDAwMDAwMDAwMDAwMDBiBgNVHSAEWzBZMFcGBmBMAQIBYDBNMEsGCCsGAQUFBwIBFj9odHRwOi8vY2NkLmFjc29sdXRpLmNvbS5ici9kb2NzL2RwYy1hYy1jZXJ0aWZpY2FtaW5hcy1zbWltZS5wZGYwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMEMIGWBgNVHR8EgY4wgYswQ6BBoD+GPWh0dHA6Ly9jY2QuYWNzb2x1dGkuY29tLmJyL2xjci9hYy1jZXJ0aWZpY2FtaW5hcy1zbWltZS12NS5jcmwwRKBCoECGPmh0dHA6Ly9jY2QyLmFjc29sdXRpLmNvbS5ici9sY3IvYWMtY2VydGlmaWNhbWluYXMtc21pbWUtdjUuY3JsMB0GA1UdDgQWBBQea4XMqPpMnhZ51xuMmdCYXGy/wTAOBgNVHQ8BAf8EBAMCBeAwDQYJKoZIhvcNAQELBQADggIBAABmuGV+ucZwOL/yNqfbim1+qwYIOi2AqsO6D3k7t1M1dZM7jl+C6MtEzkAMrnUtxarh41uGHAE1bJrPNZZFCWZBI1K0wc6sJOHFyLzLcPH9bS7k4g55/rzQ21OVr8JkHYg5nncgeQoH5KD7y2xU724W0l3YI6W6W58ew7k0msUFR+eTbjow9sk9pC4VRJdcoSYwsUSQs8LvA5sq7S4BswTYjHhp5C6jJPU3h309GbevJ1x9F/sy8OjA6/DvU19rUB99GAZIJBieXvSo5/pSHCHL8YAYEujRs+hg4QePHU71NYkKKqUime13L2deXGmNqa9a6dFxp9nz6h/9giCGFh8SeWxaHT0OesGBuP0GF3r47AcQUunk5jnShkHFLrZ8znn+I3fWBBVUlU2e8TI7bKNLAlyN8+fabv2OQlSyuk47U18VqV+c38hmtXtvDy+RK87HsaHuZljld2TPuhURD56etKwXKABs/ufSUlTx5Avk36ux/amVWfp4STsuxwIEy9/QfJLgNtHfcpjaB4AceYn1TsNK+K8XurzPuuJmB6k77ToWsMxIXdw1jasfpTTU81mrEp1gaPgZNAZ9W80DBnDiGUjsIsHRBm6JpoC1cmrPp8kYOmjwctb0TyvgIUxiSpaaW5GQ9iOGDXgLkVRs40jt4KkFgCEnM02DNDx09LOb</X509Certificate>
				</X509Data>
			</KeyInfo>
		</Signature>
	</CTe>
	<protCTe versao="3.00">
		<infProt>
			<tpAmb>1</tpAmb>
			<verAplic>MT20200708016</verAplic>
			<chCTe>51230308814422000102570010000009071415185476</chCTe>
			<dhRecbto>2023-03-18T15:03:30-04:00</dhRecbto>
			<nProt>151230601799575</nProt>
			<digVal>tDFH20nyPp2EqHInZHa/MXy+1Y0=</digVal>
			<cStat>100</cStat>
			<xMotivo>Autorizado o uso do CT-e</xMotivo>
		</infProt>
	</protCTe>
</cteProc>
endcontent

Return cXMLCTEDemo

#INCLUDE "CXTeste.ch" //Strings STR para testes em fontes padr„o
Static	__cNomeFuncao	:= 'PMSXFUN' //Nome da funÁ„o para obter os textos STR
Static Function ParamBox(aParametros,cTitle,aRet,bOk,aButtons,lCentered,nPosx,nPosy, oDlgWizard, cLoad, lCanSave,lUserSave)

Local nx
Local oDlg
Local cPath     := ""
Local oPanel
Local oPanelB
Local cTextSay
Local lOk			:= .F.
Local nLinha		:= 8
Local cArquivos := ""
Local nBottom
Local oFntVerdana
Local cOpcoes	:=	""
Local lWizard  := .F.
Local cBlkWhen2
Local nPos
Local cRotina
Local cAux
Local aOpcoes
Local cAlias1
Local cServidor		:= ".T."
Local cWhen	:= ""
Local cCodUsr := ""
Local lGrpAdm := .F.
Local loMainWnd := .F.
Local cFilAN7	:= xFilial("AN7")

DEFAULT bOk			:= {|| (.T.)}
DEFAULT aButtons	:= {}
DEFAULT lCentered	:= .T.
DEFAULT nPosX		:= 0
DEFAULT nPosY		:= 0
DEFAULT cLoad     := ProcName(1)
DEFAULT lCanSave	:= .T.
DEFAULT lUserSave	:= .F.
DEFAULT aButtons	:= {}

cRotina := PADR(cLoad,10)

If Type("cCadastro") == "U"
	cCadastro := ""
EndIf

If !lCanSave
	lUserSave	:= .F.
	cLoad := "99_NOSAVE_"
Else
	//Se nao esta bloqueado
	If ParamLoad(cLoad,aParametros,0,"1")== "2"
		lUserSave:= .F.
	//Se o usuario pode ter a sua propria configuracao
	ElseIf lUserSave
		cLoad	:=	__cUserID+"_"+cLoad
	Endif
Endif

DEFINE FONT oFntVerdana NAME "Verdana" SIZE 0, -10 BOLD

If oDlgWizard == NIL

	If Type("oMainWnd") == "U"
		DEFINE MSDIALOG oDlg TITLE cCadastro+" - "+cTitle FROM nPosX,nPosY TO nPosX+300,nPosY+445 Pixel
		loMainWnd := .F.
	Else
		If IsInCallStack("Pms320Per") .OR. IsInCallStack("P320ExPer")
			DEFINE MSDIALOG oDlg TITLE cCadastro+" - "+cTitle FROM nPosX,nPosY TO nPosX+300,nPosY+500 OF oMainWnd Pixel
		Else
			DEFINE MSDIALOG oDlg TITLE cCadastro+" - "+cTitle FROM nPosX,nPosY TO nPosX+300,nPosY+445 OF oMainWnd Pixel
		EndIf
		loMainWnd := .T.
	EndIF
	lWizard := .F.
Else
	oDlg := oDlgWizard
	lWizard := .T.
EndIf

oPanel := TScrollBox():New( oDlg, 8,10,104,203)
oPanel:Align := CONTROL_ALIGN_ALLCLIENT

For nx := 1 to Len(aParametros)
	Do Case
		Case aParametros[nx,1]==1 // SAY + GET
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3],Iif(Len(aParametros[nx])>9,aParametros[nx,10],.F.))
			EndIf
			if aParametros[nx,9] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			cWhen	:= Iif(Empty(aParametros[nx,7]),".T.",aParametros[nx,7])
			cValid	:=Iif(Empty(aParametros[nx,5]),".T.",aParametros[nx,5])
			cF3		:=Iif(Empty(aParametros[nx,6]),NIL,aParametros[nx,6])
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlKVld := "{|| "+cValid+"}"
			cBlKWhen := "{|| "+cWhen+"}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			//*****************************************************
			// Auto Ajusta da Get para Campos Caracter e Numerico *
			// Somente para o Modulo PCO - Acacio Egas            *
			//*****************************************************
			If Type("cModulo")=="C" .and. cModulo=="PCO" .and. !lWizard
				cType := Type("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				If cType $ "C"
					nWidth	:= CalcFieldSize(cType,Len(aParametros[nx,3]),,aParametros[nx,4],"") + 10 + If(!Empty(cF3),10,0)
				ElseIf cType $ "N"
					nWidth	:= CalcFieldSize(cType,,,aParametros[nx,4],"") + 10
				Else
					nWidth	:= aParametros[nx,8]
				EndIf
			Else
				nWidth	:= aParametros[nx,8]
			EndIf
			// 'If' para corrigir um problema do campo get quando possui F3 (Lupa) em um panel do wizard. Quando campo menor que 50, a lupa some.
			If lWizard
				IF Type("nWidth")<> "U"
					TGet():New( nLinha,100,&cBlKGet,oPanel,If(nWidth<30,30,nWidth),,aParametros[nx,4], &(cBlkVld),,,, .T.,, .T.,, .T., &(cBlkWhen), .F., .F.,, .F., .F. ,cF3,"MV_PAR"+AllTrim(STRZERO(nx,2,0)),,,,.T.)
				Else
					cType := ValType(aRet[nx])
					nWidth := ParBGetSize(cType,aParametros,cF3,nx)
					TGet():New( nLinha,100,&cBlKGet,oPanel,nWidth,,aParametros[nx,4], &(cBlkVld),,,, .T.,, .T.,, .T., &(cBlkWhen), .F., .F.,, .F., .F. ,cF3,"MV_PAR"+AllTrim(STRZERO(nx,2,0)),,,,.T.)
				Endif
			Else
				TGet():New( nLinha,100,&cBlKGet,oPanel,nWidth,,aParametros[nx,4], &(cBlkVld),,,, .T.,, .T.,, .T., &(cBlkWhen), .F., .F.,, .F., .F. ,cF3,"MV_PAR"+AllTrim(STRZERO(nx,2,0)),,,,.T.)
			Endif
		Case aParametros[nx,1]==2 // SAY + COMBO
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			
    		if aParametros[nx,7] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			
			cWhen   := ".T."
			If Len(aParametros[nx]) > 7
				If aParametros[nx,8] != NIL .And. ValType(aParametros[nx,8])=="L"
					cWhen	:=If(aParametros[nx,8],".T.",".F.")
				Else
					cWhen	:= Iif(Len(aParametros[nx]) < 8 .Or. Empty(aParametros[nx,8]) .Or. aParametros[nx,8] == Nil,".T.",aParametros[nx,8])
				EndIf
			EndIf
			cValid	:=Iif(Empty(aParametros[nx,6]),".T.",aParametros[nx,6])
			cBlKVld := "{|| "+cValid+"}"
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
         Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlkWhen := "{|| "+cWhen+" }"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			TComboBox():New( nLinha,100, &cBlkGet,aParametros[nx,4], aParametros[nx,5], 10, oPanel, ,,       ,,,.T.,,,.F.,&(cBlkWhen),.T.,,,,"MV_PAR"+AllTrim(STRZERO(nx,2,0)))

		Case aParametros[nx,1]==3 // SAY + RADIO
			nLinha += 8
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			cTextSay:= "{||'"+aParametros[nx,2]+" ? "+"'}"
			TGroup():New( nLinha-8,15, nLinha+(Len(aParametros[nx,4])*9)+7,205,aParametros[nx,2]+ " ? ",oPanel,If(aParametros[nx,7],CLR_HBLUE,CLR_BLACK),,.T.)
			cWhen   := ".T."
			If Len(aParametros[nx]) > 7
				If aParametros[nx,8] != NIL .And. ValType(aParametros[nx,8])=="L"
					cWhen	:=If(aParametros[nx,8],".T.",".F.")
				Else
					cWhen	:= Iif(Len(aParametros[nx]) < 8 .Or. Empty(aParametros[nx,8]) .Or. aParametros[nx,8] == Nil,".T.",aParametros[nx,8])
				EndIf
			EndIf
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
            Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlkWhen := "{|| " + cWhen  +  "}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			TRadMenu():New( nLinha, 30, aParametros[nx,4],&cBlkGet, oPanel,,,,,,,&(cBlkWhen),aParametros[nx,5],9, ,,,.T.)
			nLinha += (Len(aParametros[nx,4])*10)-3

		Case aParametros[nx,1]==4 // SAY + CheckBox
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			if aParametros[nx,7] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+"  "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+"  "+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			cBlkWhen := Iif(Len(aParametros[nx]) > 7 .And. !Empty(aParametros[nx,8]),aParametros[nx,8],"{|| .T. }")
			If (Len(aParametros[nx]) > 6 .And. aParametros[nx,7]).Or. ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			TCheckBox():New(nLinha,100,aParametros[nx,4], &cBlkGet,oPanel, aParametros[nx,5],10,,,,,,,,.T.,,,&(cBlkWhen))

		Case aParametros[nx,1]==5 // CheckBox Linha Inteira
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
            Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlkWhen := "{|| .T. }"
			If (Len(aParametros[nx]) > 6 .And. aParametros[nx,7]) .Or. ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			TCheckBox():New(nLinha,15,aParametros[nx,2], &cBlkGet,oPanel, aParametros[nx,4],10,,,,,,,,.T.,,,&(cBlkWhen))

		Case aParametros[nx,1]==6 // File + Procura de Arquivo
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			
			if aParametros[nx,8] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			
			cWhen	    := Iif(Empty(aParametros[nx,6]),".T.",aParametros[nx,6])
			cValid	  := Iif(Empty(aParametros[nx,5]),".T.","("+aParametros[nx,5]+").Or.Vazio("+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+")")
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
            Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlKVld   := "{|| " + cValid + "}"
			cBlKWhen  := "{|| " + cWhen + "}"
			
			cArquivos := aParametros[nx,9]
			If ValType(cArquivos) <> "C"
				cArquivos := ""
			EndIf
			
			If Len(aParametros[nx]) >= 10
				cPath := aParametros[nx,10]
				If ValType(cPath) <> "C"
					cPath := ""
				EndIf 
			EndIf

			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf

			If Len(aParametros[nX]) >= 11
				If ValType(aParametros[nx,11]) <> "N"
					cOpcoes := AllTrim(Str(GETF_LOCALHARD+GETF_LOCALFLOPPY))
				Else
					cOpcoes := AllTrim(Str(aParametros[nx,11]))
				EndIf
			Else
				cOpcoes := AllTrim(Str(GETF_LOCALHARD+GETF_LOCALFLOPPY))
			EndIf

			If Len(aParametros[nX]) >= 12
				cServidor := cValToChar(aParametros[nx,12])
			Else
				cServidor := ".T." 
			Endif

			If lWizard
				cGetfile := "{|| aRet["+AllTrim(STRZERO(nx,2,0))+"] := MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := cGetFile('"+cArquivos+"','"+;
								STR0176+"',0,'"+cPath+"',.T.,"+cOpcoes+;
								","+cServidor+")+SPACE(40), If(Empty(MV_PAR"+AllTrim(STRZERO(nx,2,0))+;
								"), MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := '"+;
								aParametros[nx,3]+"',)  }"
		 	Else
				cGetfile := "{|| MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := cGetFile('"+cArquivos+"','"+;
								STR0176+"',0,'"+cPath+"',.T.,"+cOpcoes+;
								","+cServidor+")+SPACE(40), If(Empty(MV_PAR"+AllTrim(STRZERO(nx,2,0))+;
								"), MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := '"+;
								aParametros[nx,3]+"',)  }" 
			EndIf

			TGet():New( nLinha,100 ,&cBlKGet,oPanel,aParametros[nx,7],,aParametros[nx,4], &(cBlkVld),,,, .T.,, .T.,, .T., &(cBlkWhen), .F., .F.,, .F., .F. ,,"MV_PAR"+AllTrim(STRZERO(nx,2,0)))
			TButton():New( nLinha,100+aParametros[nx,7], STR0175, oPanel,&(cGetFile), 29, 12, , oDlg:oFont, ,.T.,.F.,,.T., ,, .F.)
 
		Case aParametros[nx,1]==7 //.And. ! lWizard// Filtro de Arquivos
			nLinha += 8
			If !lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,4])
				SetPrvt("MV_FIL"+AllTrim(STRZERO(nx,2,0)))
				&("MV_FIL"+AllTrim(STRZERO(nx,2,0))) := MontDescr(aParametros[nx,3],ParamLoad(cLoad,aParametros,nx,aParametros[nx,4]))
			EndIf
			TGroup():New( nLinha-8,15, nLinha+40,170,aParametros[nx,2]+ " ? ",oPanel,,,.T.)
			cWhen   := ".T."
			If Len(aParametros[nx]) > 4
				If aParametros[nx,5] != NIL .And. ValType(aParametros[nx,5])=="L"
					cWhen	:=If(aParametros[nx,5],".T.",".F.")
				Else
					cWhen	:= Iif(Len(aParametros[nx]) < 5 .Or. Empty(aParametros[nx,5]) .Or. aParametros[nx,5] == Nil,".T.",aParametros[nx,5])
				EndIf
			EndIf
			cValid	:=".T."
			If !lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_FIL"+AllTrim(STRZERO(nx,2,0))+","+"MV_FIL"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			Else
				cBlkGet := "{ | u | If( PCount() == 0, MontDescr('"+aParametros[nx,3]+"',aRet["+AllTrim(STRZERO(nx,2,0))+"]),"+;
																	" MV_FIL"+AllTrim(STRZERO(nx,2,0))+":= u ) }"

			EndIf
			cBlKVld := "{|| "+cValid+"}"
			cBlKWhen := "{|| "+cWhen+"}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			If !lWizard
				cGetFilter := "{|| MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := BuildExpr('"+aParametros[nx,3]+"',,MV_PAR"+AllTrim(STRZERO(nx,2,0))+"),MV_FIL"+AllTrim(STRZERO(nx,2,0))+":=MontDescr('"+aParametros[nx,3]+"',MV_PAR"+AllTrim(STRZERO(nx,2,0))+") }"
			Else
				cGetFilter := "{|| aRet["+AllTrim(STRZERO(nx,2,0))+"] := MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := BuildExpr('"+aParametros[nx,3]+"',,aRet["+AllTrim(STRZERO(nx,2,0))+"]),MV_FIL"+AllTrim(STRZERO(nx,2,0))+":=MontDescr('"+aParametros[nx,3]+"',aRet["+AllTrim(STRZERO(nx,2,0))+"]) }"
			EndIf
			TButton():New( nLinha,18, "Editar", oPanel,&(cGetFilter), 35, 14, , oDlg:oFont, ,.T.,.F.,,.T.,&(cBlkWhen),, .F.)
			TMultiGet():New( nLinha, 55, &cBlKGet,oPanel,109,33,,,,,,.T.,,.T.,&(cBlkWhen),,,.T.,&(cBlkVld),,.T.,.F., )
			nLinha += 31
		Case aParametros[nx,1]==8 // SAY + GET PASSWORD
			If ! lWizard
				SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
				&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			EndIf
			if aParametros[nx,9] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+"'}"
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			
			cWhen	:= Iif(Empty(aParametros[nx,7]),".T.",aParametros[nx,7])
			cValid	:=Iif(Empty(aParametros[nx,5]),".T.",aParametros[nx,5])
			cF3		:=Iif(Empty(aParametros[nx,6]),NIL,aParametros[nx,6])
			If ! lWizard
				cBlkGet := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
            Else
				cBlkGet := "{ | u | If( PCount() == 0, "+"aRet["+AllTrim(STRZERO(nx,2,0))+"],"+"aRet["+AllTrim(STRZERO(nx,2,0))+"] := "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			EndIf
			cBlKVld := "{|| "+cValid+"}"
			cBlKWhen := "{|| "+cWhen+"}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			TGet():New( nLinha,100 ,&cBlKGet,oPanel,aParametros[nx,8],,aParametros[nx,4], &(cBlkVld),,,, .T.,, .T.,, .T., &(cBlkWhen), .F., .F.,, .F., .T. ,cF3,"MV_PAR"+AllTrim(STRZERO(nx,2,0)),,,,.T.)
		Case aParametros[nx,1]==9 // SAY
            cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+"'}"
			If aParametros[nx,5]
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,oFntVerdana,,,,.T.,CLR_BLACK,,aParametros[nx,3],aParametros[nx,4],,,,,)
			Else
				TSay():New( nLinha, 15 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,aParametros[nx,3],aParametros[nx,4],,,,,)
			EndIf
		Case aParametros[nx,1]==10 // Range (fase experimental)
			nLinha += 8
			SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
			&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			SetPrvt("MV_RAN"+AllTrim(STRZERO(nx,2,0)))
			&("MV_RAN"+AllTrim(STRZERO(nx,2,0))) := PMSRangeDesc(	&("MV_PAR"+AllTrim(STRZERO(nx,2,0))),aParametros[nx,7])
			TGroup():New( nLinha-8,15, nLinha+40,170,STR0382+aParametros[nx,2],oPanel,,,.T.)		//"Range de "
			If Type(aParametros[nx,8])=="L" .And. !Empty(aParametros[nx,8])
				cWhen	:= aParametros[nx,8]
			Else
				cWhen	:= ".T."
			EndIf
			cValid	:=".T."
			cBlkGet := "{ | u | If( PCount() == 0, "+"MV_RAN"+AllTrim(STRZERO(nx,2,0))+","+"MV_RAN"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			cBlKWhen := "{|| "+cWhen+"}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			cGetRange := "{|| MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := PmsRange('"+aParametros[nx,2]+"','"+aParametros[nx,4]+"',"+Str(aParametros[nx,5])+",MV_PAR"+AllTrim(STRZERO(nx,2,0))+",'"+aParametros[nx,6]+"',"+Str(aParametros[nx,7])+"),	MV_RAN"+AllTrim(STRZERO(nx,2,0))+" := PMSRangeDesc( MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+Str(aParametros[nx,7])+") }"
	   		TButton():New( nLinha-2,18, STR0381, oPanel,MontaBlock(cGetRange), 35, 14, , oDlg:oFont, ,.T.,.F.,,.T.,&(cBlkWhen),, .F.) //"Editar"
			TMultiGet():New( nLinha, 55, &cBlKGet,oPanel,109,33,,,,,,.T.,,.T.,&(cBlkWhen),,,.T.,/*&(cBlkVld)*/,,.T.,.F., )
			nLinha += 31
		Case aParametros[nx,1]==11 // MULTIGET - campo memo
			nLinha += 10
			SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
			&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,3])
			TGroup():New( nLinha-8,15, nLinha+40,170,"",oPanel,,,.T.)
			if aParametros[nx,6] // Campo Obrigatorio
				cTextSay :="{||'<b>"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+ "<font color=red size=2 face=verdana,helvetica>*</font></b>"+"'}"
				TSay():New( nLinha - 6, 23 , MontaBlock(cTextSay)  , oPanel , ,,,,,.T.,CLR_BLACK,,100,  ,,,,,,.T.)
			else
				cTextSay:= "{||'"+STRTRAN(aParametros[nx,2],"'",'"')+" ? "+"'}"
				TSay():New( nLinha - 6, 23 , MontaBlock(cTextSay) , oPanel , ,,,,,.T.,CLR_BLACK,,100,,,,,,)
			endif	
			
			cValid := Iif(Empty(aParametros[nx,4]),".T.",aParametros[nx,4])
			cWhen  := Iif(Empty(aParametros[nx,5]),".T.",aParametros[nx,5])
			cBlkGet  := "{ | u | If( PCount() == 0, "+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+","+"MV_PAR"+AllTrim(STRZERO(nx,2,0))+":= u ) }"
			cBlkVld  := "{|| " + cValid + "}"
			cBlkWhen := "{|| " + cWhen + "}"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			Endif
			TMultiGet():New(nLinha+1,23,&cBlkGet,oPanel,140,33,/*oFont*/,/*lHScroll*/,/*nClrFore*/,/*nClrBack*/,/*oCursor*/,.T.,/*cMg*/,;
			.T.,&(cBlkWhen),/*lCenter*/,/*lRight*/,.F.,&(cBlkVld),/*bChange*/,.T.,.F.)
			nLinha += 31
		Case aParametros[nx,1]==12 // FILTROS DE USUARIO POR ROTINA
			nLinha += 8
			SetPrvt("MV_FIL"+AllTrim(STRZERO(nx,2,0)))
			If len(aParametros[nx])>3
				&("MV_FIL"+AllTrim(STRZERO(nx,2,0))) := ParamLoad(cLoad,aParametros,nx,aParametros[nx,4])
			Else
				&("MV_FIL"+AllTrim(STRZERO(nx,2,0))) := ""
			EndIf
			SetPrvt("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
			&("MV_PAR"+AllTrim(STRZERO(nx,2,0))) := ""
			cTextSay := ""
			If Len(aParametros[nx]) > 1
				If aParametros[nx,2] != Nil .And. ValType(aParametros[nx,2])=="C"
					cTextSay := aParametros[nx,2]
				EndIf
			Else
				AADD(aParametros[nx], "")
			EndIf
			cAlias1 := ""
			If Len(aParametros[nx]) > 2
				If aParametros[nx,3] != Nil .And. ValType(aParametros[nx,3])=="C"
					cAlias1	:= aParametros[nx,3]
				EndIf
			Else
				AADD(aParametros[nx], "")
			EndIf
			If empty(cAlias1)
				If PcoX2ConPad(cAlias1)
					cAlias1 := PcoSX2Cons()
				Else
					cAlias1 := ALIAS()
				EndIf
			EndIf
			If empty(aParametros[nx,3])
				aParametros[nx,3] := cAlias1
			EndIf
			cWhen   := ".T."
			If Len(aParametros[nx]) > 4
				If aParametros[nx,5] != Nil .And. ValType(aParametros[nx,5])=="L"
					cWhen	:= If(aParametros[nx,5],".T.",".F.")
				EndIf
			EndIf
			cBlkWhen := "{|| "+cWhen+" }"
			If ParamLoad(cLoad,aParametros,0,"1")=="2"
				cBlKWhen := "{|| .F. }"
			EndIf
			aOpcoes := {"Visualizar todos os registros"}
			cBlkWhen2:=cBlKWhen
			dbSelectArea("AN7")
			AN7->(dbSetOrder(1))
			AN7->(MsSeek(cFilAN7+oApp:cUserID+cRotina+cAlias1))
			Do While !AN7->(Eof()) .And. AN7->(AN7_FILIAL+AN7_USER+AN7_FUNCAO+AN7_ALIAS)==cFilAN7+oApp:cUserID+cRotina+cAlias1
				AADD(aOpcoes, AN7->AN7_FILTR)
				AN7->(dbSkip())
			EndDo
			TGroup():New( nLinha-8,15, nLinha+20,170, cTextSay,oPanel,,,.T.)
			cBlKVld := "{|| .T.}"
			cBlkGet := "{ | u | If( PCount() == 0, MV_FIL"+AllTrim(STRZERO(nx,2,0))+", MV_FIL"+AllTrim(STRZERO(nx,2,0))+":= u) }"
			SetPrvt("oCombo"+AllTrim(STRZERO(nx,2,0)))
			&("oCombo"+AllTrim(STRZERO(nx,2,0))) := TComboBox():New( nLinha+4, 20, &cBlkGet, aOpcoes, 100, 10, oPanel,,,,,,.T.,,,.F.,&(cBlkWhen),.T.,,,,"MV_FIL"+AllTrim(STRZERO(nx,2,0)))

			cAux := "{|| MV_PAR"+AllTrim(STRZERO(nx,2,0))+" := PmsGetFilt( oApp:cUserID, cRotina, '"+cAlias1+"', MV_FIL"+AllTrim(STRZERO(nx,2,0))+" )}"
	   		TBtnBmp2():New( (nLinha+4)*2, 120*2, 25, 25, "FILTRO1"  , , , , &cAux , oPanel, "Aplicar filtro selecionado", &(cBlkWhen), )
			cAux := "{|| PmsIncFilt( aParametros, oApp:cUserID, cRotina, '"+cAlias1+"' )}"
	   		TBtnBmp2():New( (nLinha+4)*2, 132*2, 25, 25, "BPMSDOCI" , , , , &cAux , oPanel, "Novo filtro", &(cBlkWhen2), )
			cAux := "{|| PmsAltFilt( aParametros, oCombo"+AllTrim(STRZERO(nx,2,0))+":nAt, oApp:cUserID, cRotina, '"+cAlias1+"', MV_FIL"+AllTrim(STRZERO(nx,2,0))+" )}"
	   		TBtnBmp2():New( (nLinha+4)*2, 144*2, 25, 25, "BPMSDOCA" , , , , &cAux , oPanel, "Editar filtro selecionado", &(cBlkWhen2), )
			cAux := "{|| PmsExcFilt( aParametros, oCombo"+AllTrim(STRZERO(nx,2,0))+":nAt, oApp:cUserID, cRotina, '"+cAlias11+"', MV_FIL"+AllTrim(STRZERO(nx,2,0))+" )}"
	   		TBtnBmp2():New( (nLinha+4)*2, 156*2, 25, 25, "BPMSDOCE" , , , , &cAux , oPanel, "Excluir o filtro selecionado", &(cBlkWhen2), )
			nLinha += 11
    EndCase
	nLinha += 17
Next


lGrpAdm := .F.
cCodUsr := RetCodUsr()
If !Empty(cCodUsr)
	lGrpAdm := PswAdmin( /*cUser*/, /*cPsw*/,cCodUsr)==0
EndIf

If !lWizard .And.  lGrpAdm .And. lCanSave
	@ nlinha+8,10 BUTTON oButton PROMPT "+" SIZE 10 ,7   ACTION {|| ParamSave(cLoad,aParametros,"1") } OF oPanel PIXEL
	@ nlinha+8,22 SAY STR0307 SIZE 120,7 Of oPanel FONT oFntVerdana COLOR RGB(80,80,80) PIXEL //"Administrador: Salvar configuaÁıes"
	oButton:cToolTip := STR0308 + cTitle //"Clique aqui para salvar as configuraÁıes de: "

	@ nlinha+15,10 BUTTON oButton PROMPT "+" SIZE 10 ,7   ACTION {|| ParamSave(cLoad,aParametros,"2"),Alert(STR0313) } OF oPanel PIXEL  //"Bloqueio efetuado. Os parametros estar„o bloqueados a partir da prÛxima chamada."
	@ nlinha+15,22 SAY STR0309 SIZE 120,7 Of oPanel FONT oFntVerdana COLOR RGB(80,80,80) PIXEL //"Administrador: Bloquear"
	oButton:cToolTip := STR0310 + cTitle //"Clique aqui para bloquear as configuraÁıes de: "

	@ nlinha+22,10 BUTTON oButton PROMPT "+" SIZE 10 ,7   ACTION {|| ParamSave(cLoad,aParametros,"1"),Alert(STR0314)  } OF oPanel PIXEL  //"Desbloqueio efetuado. Os parametros estar„o desbloqueados a partir da prÛxima chamada."
	@ nlinha+22,22 SAY STR0311 SIZE 120,7 Of oPanel FONT oFntVerdana COLOR RGB(80,80,80) PIXEL //"Administrador: Desbloquear"
	oButton:cToolTip := STR0312 + cTitle //"Clique aqui para desbloquear as configuraÁıes de: "
EndIf

If loMainWnd
	oMainWnd:CoorsUpdate()
EndIf

If ! lWizard
	oPanelB := TPanel():New(0,0,'',oDlg, oDlg:oFont, .T., .T.,, ,40,20,.T.,.T. )
	oPanelB:Align := CONTROL_ALIGN_BOTTOM

	For nx := 1 to Len(aButtons)
		SButton():New( 4, 157-(nx*33), aButtons[nx,1],aButtons[nx,2],oPanelB,.T.,IIf(Len(aButtons[nx])==3,aButtons[nx,3],Nil),)
	Next
	//DEFINE SBUTTON FROM 4, 114   TYPE 4 ENABLE OF oDlg ACTION ParamSave(cLoad,aParametros)
	DEFINE SBUTTON FROM 4, 157   TYPE 1 ENABLE OF oPanelB ACTION (If(ParamOk(aParametros,@aRet).And.Eval(bOk),(oDlg:End(),lOk:=.T.),(lOk:=.F.)))
	DEFINE SBUTTON FROM 4, 190   TYPE 2 ENABLE OF oPanelB ACTION (lOk:=.F.,oDlg:End())
	If loMainWnd .AND. (nLinha*2) + 80 > oMainWnd:nBottom-oMainWnd:nTop
		nBottom  := oDLg:nTop + oMAinWnd:nBottom-oMAinWnd:nTop - 105
	Else
		nBottom := oDLg:nTop + (nLinha*2) + 80
	EndIf
	nBottom := MAX(310,nBottom)
	oDlg:nBottom := nBottom
EndIf
If ! lWizard
	ACTIVATE MSDIALOG oDlg CENTERED
	If lOk .And. lUserSave
		ParamSave(cLoad,aParametros,"1")
	Endif
EndIf
Return lOk


Static Function ParamSave(cLoad,aParametros,cBloq)
local nx

Local cWrite := cBloq+"Arquivo de configuraÁ„o - Parambox Protheus "+CRLF
Local cBarra := If(issrvunix(), "/", "\")

For nx := 1 to Len(aParametros)
	Do Case
		Case ValType(&("MV_PAR"+AllTrim(STRZERO(nx,2,0)))) == "C"
			cWrite += "C"+&("MV_PAR"+AllTrim(STRZERO(nx,2,0)))+CRLF
		Case ValType(&("MV_PAR"+AllTrim(STRZERO(nx,2,0)))) == "N"
			cWrite += "N"+Str(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))+CRLF
		Case ValType(&("MV_PAR"+AllTrim(STRZERO(nx,2,0)))) == "L"
			cWrite += "L"+If(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))),"T","F")+CRLF
		Case ValType(&("MV_PAR"+AllTrim(STRZERO(nx,2,0)))) == "D"
			cWrite += "D"+DTOC(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))+CRLF
		OtherWise
			cWrite += "X"+CRLF
	EndCase
Next
If !ExistDir(cBarra + "PROFILE")
	MakeDir(cBarra + "PROFILE")
EndIf
MemoWrit(cBarra + "PROFILE" + cBarra +Alltrim(cLoad)+".PRB",cWrite)
Return

Static Function ParamLoad(cLoad,aParametros,nx,xDefault,lDefault)
local ny
Local cBarra 		:= If(issrvunix(), "/", "\")
Local cTypeData 	:= NIL
DEFAULT lDefault 	:= .F.

If File(cBarra + "PROFILE" + cBarra +Alltrim(cLoad)+".PRB")
	If FT_FUse(cBarra +"PROFILE"+cBarra+Alltrim(cLoad)+".PRB")<> -1
		FT_FGOTOP()
		If nx == 0
			cLinha := FT_FREADLN()
			FT_FUSE()
			Return Substr(cLinha,1,1)
		EndIf
		For ny := 1 to nx
			FT_FSKIP()
		Next
		cLinha := FT_FREADLN()
		If !lDefault
			cTypeData := Valtype(xDefault)
			Do case
				Case Substr(cLinha,1,1) == "L" .And. cTypeData == "L"
					xRet := If(Substr(cLinha,2,1)=="F",.F.,.T.)
				Case Substr(cLinha,1,1) == "D" .And. cTypeData == "D"
					xRet := CTOD(Substr(cLinha,2,10))
				Case Substr(cLinha,1,1) == "C" .And. cTypeData == "C"
					//**********************************************
					// Tratamento para aumentar o tamanha do campo *
					//**********************************************
					If VALTYPE(xDefault)=="C"
						xRet := Padr(Substr(cLinha,2,Len(cLinha)),Len(xDefault))
					Else
						xRet := Substr(cLinha,2,Len(cLinha))
					EndIf
				Case Substr(cLinha,1,1) == "N" .And. cTypeData == "N"
					xRet := Val(Substr(cLinha,2,Len(cLinha)))
				OtherWise
					xRet := xDefault
			EndCase
		Else
			xRet := xDefault
		Endif
		FT_FUSE()
	EndIf
Else
	xRet := xDefault
EndIf

Return xRet

/*/
‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±⁄ƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒø±±
±±≥FunáÖo    ≥ParamOk ≥ Autor ≥ Edson Maricate          ≥ Data ≥ 09-02-2001 ≥±±
±±√ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒ¥±±
±±≥DescriáÖo ≥Valida a digitacao de todos oa parametros.                    ≥±±
±±√ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥±±
±±≥ Uso      ≥Generico                                                      ≥±±
±±¿ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
*/
Static Function ParamOk(aParametros,aRet)
Local nx
Local lRet	:= .T.

For nx := 1 to Len(aParametros)
	Do case
		Case aParametros[nx,1]==1
			If aParametros[nx,9] .And. Empty(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))
				lRet := .F.
				HELP("   ",1,"OBRIGAT",,STR0174+aParametros[nx,2]+SPACE(40),3,0) //"Campo : "
				Exit
			EndIf
		Case aParametros[nx,1]==3
			If aParametros[nx,7] .And. Empty(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))
				lRet := .F.
				HELP("   ",1,"OBRIGAT",,STR0174+aParametros[nx,2]+SPACE(40),3,0) //"Campo : "
				Exit
			EndIf
		Case aParametros[nx,1]==6
			If aParametros[nx,8] .And. Empty(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))
				lRet := .F.
				HELP("   ",1,"OBRIGAT",,STR0174+aParametros[nx,2]+SPACE(40),3,0) //"Campo : "
				Exit
			EndIf
		Case aParametros[nx,1]==11
			If aParametros[nx,6] .And. Empty(&("MV_PAR"+AllTrim(STRZERO(nx,2,0))))
				lRet := .F.
				HELP("   ",1,"OBRIGAT",,STR0174+aParametros[nx,2]+SPACE(40),3,0) //"Campo : "
				Exit
			EndIf
	EndCase
Next

If lRet
	aRet := Array(Len(aParametros))
	For nx := 1 to Len(aParametros)
		aRet[nx] := &("MV_PAR"+AllTrim(STRZERO(nx,2,0)))
	Next
EndIf

Return lRet

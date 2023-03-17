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
//##|Desc.   | Tela de conferência das informações fiscais de documentos de entrada (SF1/SD1)    |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|03/03/23| Cirilo R.| Consegui colocar uma enchoice na SFT                                   |##
//##|        |          | Adicionados campo natureza                                             |##
//##|08/03/23| Cirilo R.| Tratamento para dados sensíveis                                        |##
//##|16/03/23| Cirilo R.| Conversão para ParamBox                                                |##
//##|        |          | Adicionado botão conhecimento (MPDocument)                             |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################

////////////FILTROS RELACIONAIS COM OUTRAS TABELAS AINDA NÃO ESTÃO FUNCIONANDO

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
	Private cCadastro	:= 'Conferência Fiscal'		AS Character
	Private cDirTmp		:= GetTempPath(.T.)			AS Character	//Diretório temporário do cliente
	Private lTelaObs	:= .F.						AS Logical		//Controle para não abrir duas vezes a tela
	Private oBrw    								AS Object
	Private oDocXML		:= tCXXMLManager():New()	AS Object		//Otimização CXLibXMLNFeCTe
	Private oModelCache								AS Object

	//Parâmetros de filtro
	Private cFilDe									AS Character
	Private cFilAte									AS Character
	Private dLancDe									AS Date
	Private dLancAte								AS Date
	Private lSoPend									AS Logical

	//Posições array aXRotina
	Private cRX_DESC	:= 01						AS Integer
	Private bRX_EXEC	:= 02						AS Integer
	Private nRX_NOPC	:= 04						AS Integer
	Private nRX_KEY		:= 05						AS Integer
	Private nRX_VIEW	:= 06						AS Integer

	//Inicializa Variaveis-------------------------------------------------------------------------
	//Campos que não tem no dicionário coloco um equivalente no dicionário para funcionar
	aCpoSub	:= {{'USR_NOME'	,'AD1_DSCUSR'},;
				{'D1_VALLIQ','BAF_VLRLIQ'},;
				{'D1_RECNO' ,'CU0_RECNO'},;
				{'F1_RECNO' ,'CU0_RECNO'},;
				{'D1_TIPOCF','F0M_TPCF'};
				}

	//Campos da query, ter cuidado caso alterar a query alterar aqui também (ORDEM DOS CAMPOS)
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
	aAdd(aParamBox,{1,'Filial De' 		, ' ',,,'XM0',,020,.F.})
	aAdd(aParamBox,{1,'Filial Até' 		,'ZZ',,,'XM0',,020,.T.})
	aAdd(aParamBox,{1,'Dt. Lanç. De' 	,CtoD("01/01/08"),,,,,060,.T.})
	aAdd(aParamBox,{1,'Dt. Lanç. Até' 	,CtoD("01/01/40"),,,,,060,.T.})
	aAdd(aParamBox,{2,'Apenas Não Conf.',1,{'Sim','Não'},060,,.T.})

	If .Not. ParamBox(aParamBox,U_CXTxtMsg()+'Informe os parâmetros filtro')
		Return
	EndIf

	cFilDe		:= MV_PAR01
	cFilAte		:= MV_PAR02
	dLancDe		:= MV_PAR03
	dLancAte	:= MV_PAR04
	lSoPend		:= ( MV_PAR05 == 1)

	If ( dLancAte - dLancDe ) > 30
		ApMsgAlert(	'NÃO É POSSÍVEL INFORMAR UM PERÍODO MAIOR QUE 30 DIAS.'+CRLF+;
					'POR QUESTÕES DE DESEMPENHO DO SISTEMA.',U_CXTxtMsg(,,.T.))
		Return
	EndIf

	//Rotinas utilizadas no browse, tela e atalhos-------------------------------------------------
	//              Descrição[1]             Rotina[2]                      nOpc[4]      Atalho[5]	View[6]
	aAdd(aXRotina,{ "Con&ferência (F2)"		,{|| TelaConferencia() }		,,nOPC_VISUAL , VK_F2	,.F.})// feito assim porque não aceita usar 'VIEW.RFISA03'
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
	aAdd(aXRotina,{"&Observações (F3)",aMenuObs								,,nOPC_VISUAL, NIL ,.F.})

	//Teclas de atalho-----------------------------------------------------------------------------
	aEval(aXRotina,{|x| IIF(x[nRX_KEY]<> NIL,SetKey(x[nRX_KEY],x[bRX_EXEC]),)})
	SetKey(VK_F3,{|| TelaObs() })

	//---------------------------------------------------------------------------------------------
	oBrw	:= tCXMBrowse():New()
	oBrw:CXSetFields(aCampos,aCpoSub)
	oBrw:SetObfuscFields({'USR_NOME','B1_DESC'})

	//Adiciona a legenda (DEIXAR A LEGENDA AQUI PARA SER A PRIMEIRA COLUNA)
	oBrw:AddLegend( "Empty(F1_YUSRCFG)"		, "BR_VERDE"	,"NÃO Conferido")
	oBrw:AddLegend( ".Not.Empty(F1_YUSRCFG)", "BR_VERMELHO"	,"Conferido"	)

	oBrw:CXSetQuery(RetQuery())
	oBrw:CXBindParam({cFilDe,cFilAte,DtoS(dLancDe),DtoS(dLancAte)})
	ProcCol(oBrw)	//Processa coluna F1_TIPO porque não tem combo no SX3
	oBrw:CXSetQueryIndex({'F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+D1_COD+D1_ITEM'})
	//oBrw:disableDetails()
	//oBrw:disableFilter()
	//oBrw:disableSeek()
	//Adiciona filtro Relacional com outra tabela, observar o compartilhamento entre ambas as tabelas, se diferente não usar o campo filial.
	oBrw:_aRelation	:= {{.F.,'SF1','Cabeçalho Doc',	"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA",;
													"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA"},;
						{.F.,'SA2','Fornecedor',	"F1_FORNECE+F1_LOJA+D1_TIPOCF",;
													"A2_COD+A2_LOJA+'F'"},;
						{.F.,'SA1','Cliente'		,"F1_FORNECE+F1_LOJA+D1_TIPOCF",;
													"A1_COD+A1_LOJA+'C'"},;
						{.F.,'SFT','Livros Fiscais',"F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_ITEM+'E'",;
													"FT_FILIAL+FT_NFISCAL+FT_SERIE+FT_CLIEFOR+FT_LOJA+FT_ITEM+FT_TIPOMOV"}}

	//Adiciona botões------------------------------------------------------------------------------
	aEval(aXRotina,{|x| oBrw:AddButton(x[cRX_DESC], x[bRX_EXEC],,x[nRX_NOPC] )})
	
	oBrw:AddButton( "Legendas"		,{|| oBrw:aLegends[1][2]:View()}		,,nOPC_VISUAL )
	oBrw:AddButton( "Layout"		,U_CXBrRtCfg('oBrw')[2]					,,nOPC_ALTERA )		//Configurações de layout do browse

	oBrw:SetDescription(cCadastro)	//"Atualização de Pedidos de Venda"

	oBrw:Activate(/*oDlg*/)
	
	//Limpa arquivos temporários-------------------------------------------------------------------
	For nX := 1 to Len(aArqTmp)
		fErase(aArqTmp[nX])	//Não preciso contrar mensagens de erro porque são apenas temporários mesmo
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
//##|Desc.   | Monta a tela de conferência fiscal, feito desta forma porque os atalhos exigem um |##
//##|        |  bloco de código para execução e no menu basta a string VIEW.RFISA03, também para |##
//##|        |  proteger o atalho F2 de ser executado duas vezes e chamar uma tela por cima da   |##
//##|        |  outra causando instabilidade                                                     |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

	//FEITO AQUI DEVIDO A UM BUG QUANDO USANDO COM O MÉTODO addUserButton, SE O BOTÃO EM QUESTÃO NÃO
	// ESTIVER VISÍVEL NA TELA O SISTEMA APRESENTA UM ERRO LOG AO ACIONAR A TECLA DE ATALHO
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
	FWExecView('Conferência',"RFISA03",,,,,,aEnableButtons,,,,oModelCache)
	oModelCache:DeActivate()	//Apenas desativo o objeto, se abrir novamente o cache fica rápido

	oKeyBak:RestFKey()	//Restaura teclas Fx
	oBrw:Refresh()		//Atualiza a tela!

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| RetQuery                        | Autor | Cirilo Rocha       | Data | 27/01/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Monta a query com os dados para serem exibidos no browse                          |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
	//Cabeçalho Doc. Entrada
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
	//Usuários
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
//##|Desc.   | Ajusta a coluna F1_TIPO porque a mesma não tem combo no dicionário SX3            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

	//O campo F1_TIPO não tem combo cadastrado, então alimento manualmente aqui
	nPos		:= aScan(oBrw:aColumns,{|x| x:GetID() == 'F1_TIPO' })
	If 	nPos > 0 .And. ;
		Len(oBrw:aColumns[nPos]:GetOptions()) == 0

		nTamCpo	:= oBrw:aColumns[nPos]:GetSize()
		aCombo	:= U_CXTipoNF(2)
		aEval(aCombo,{|x| IIf(Len(x) > nTamCpo,nTamCpo:=Len(x),) } )

		oBrw:aColumns[nPos]:SetOptions(U_CXTipoNF(4))
		oBrw:aColumns[nPos]:SetSize( nTamCpo )
	EndIf

	//Mostro o campo de forma mais amigável ao usuário
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
//##|Desc.   | Abre a tela de visualização de documento de entrada                               |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| PosSF1                          | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Posiciona no registro do documento de entrada (SF1) correspondente                |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
//##|Desc.   | Abre a tela de manutenção dos livros fiscais por item                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
	Private ALTERA	:= .T.		AS Logical	//Preciso sobrepor as variáveis da rotina chamadora
	Private INCLUI	:= .F.		AS Logical

	//---------------------------------------------------------------------------------------------
	oArea	:= tCtrlAlias():GetArea({'SF3','SFT','SM0','#KEY'})
	If aRtMata900 == NIL
		aRtMata900	:= FWLoadMenuDef('aRtMata900')
	EndIf
	aRotina	:= aClone(aRtMata900)
	nOpc	:= nOPC_ALTGRD
	//6 = Alteracao sem a permissao para incluir novas linhas. Eh valido apenas para os objetos GetDados e GetDb

	//Documento já conferido
	If .Not. Empty((cAlias)->F1_YUSRCFG)
		//If .Not. ApMsgNoYes('Este documento já está marcado como conferido.'+CRLF+;
		//					'NÃO SERÁ POSSÍVEL ALTERAR OS DADOS DO LIVRO FISCAL.'+CRLF+CRLF+;
		//					'DESEJA ABRIR NO MODO SOMENTE VISUALIZAÇÃO ?',U_CXTxtMsg(,,.T.))
		//	Return
		//Else
		//	nOpc	:= nOPC_VISUAL
		//	ALTERA	:= .F.
		//EndIf
		//A ROTINA PADRÃO NÃO EXISTE ESSA OPERAÇÃO, ELE SEMPRE ALTERA
		U_CXHelp(,,	'Este documento já está marcado como conferido.'+CRLF+;
					'NÃO SERÁ POSSÍVEL ALTERAR OS DADOS DO LIVRO FISCAL.',,/*cSolucao*/)
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

			MATA917('SF3',SF3->(Recno()),nOpc)	//Esta função retorna status se clicou em OK alterando!
			
			If nOpc	== nOPC_ALTGRD
				AtualizaTmp({'SFT'})
				lAtualizou	:= .T.
			EndIf
		Else
			xMagHelpFis (U_CXTxtMsg()+"Atenção - Processo interrompido",;
			FWI18NLANG("MATA917","STR0016",0016),;	//"O Acerto Fiscal Por Item somente é permitido para Notas Fiscais que possuam escrituração fiscal por item (tabela SFT) relacionado, ou seja, que tenha vindo de um processo completo, COMPRAS ou FATURAMENTO. Neste caso, não foi encontrado tal relacionamento não premitindo a execução da opção solicitada."
			FWI18NLANG("MATA917","STR0017",0017)) 	//"A tabela de Livros Fiscais por item (SFT) necessita dos itens da Nota Fiscal (Tabela SD1 ou SD2) que neste caso não existe, pois esta Nota Fiscal pode ter sido incluída manualmente através de acertos fiscais onde não será possível tal manutenção devido somente existir informações na tabela SF3 (Livros Fiscais)."
		EndIf
	EndIf

	oArea:RestArea()		//Restaura area

	If	lAtualizou .And. ;
		FWIsInCallStack('TELACONFERENCIA')	//Veio da tela de conferência preciso atualizar

		oModelCache:DeActivate()			//Força a recarga dos dados
		oModelCache:Activate()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| PosSF3                          | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Posiciona no registro dos livros fiscais (SF3) correspondente                     |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
					'DOCUMENTO NÃO GEROU LIVROS FISCAIS.',;
					/*cSolucao*/,/*lMsg*/,/*oArea*/)
	EndIf

Return lRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| AtualizaTmp                     | Autor | Cirilo Rocha       | Data | 02/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Grava dados atualizados nas tabelas origem para a tabela temporária da tela       |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

			//Atualiza campos na tabela temporária-------------------------------------------------
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
	oBrw:Refresh()			//Atualiza a tela!

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| GetXML                          | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Função para obter o arquivo XML do documento no sistema                           |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
									oDocXML			);	//16 Object cache para otimização (def NIL)
									}
	//Apenas estas espécies tem XML
	If AllTrim((cAlias)->F1_ESPECIE) $ 'CTE/SPED'
		oArea		:= tCtrlAlias():GetArea({'SM0'})
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf
		
		SF1->(dbGoTo((cAlias)->F1_RECNO))

		//Busca nos arquivos anexados do sistema
		aArq	:= U_CXGetDoc('SF1',.F.)
		For nX := 1 to len(aArq)
			If 	Upper(Right(AllTrim(aArq[nX]),4)) == '.XML' .And. ;	//Se o tipo do arquivo e' XML
				U_CXFile( AllTrim(aArq[nX]) , .T. )					//Busca o arquivo fisico no servidor

				//Verifico se o arquivo é mesmo válido
				If eVal(bVldArq,aArq[nX])
					cFile	:= aArq[nX]
					Exit
				EndIf
			EndIf
		Next

		If Empty(cFile)			//Não achou nos arquivos anexados
			//Busca na pasta do XML salvo
			If U_CXBscXML(	(cAlias)->F1_CHVNFE	,;	//01 cChave
							@cFile				,;	//02 @cFile
							(cAlias)->F1_EMISSAO)	//03 data de emissão
				If .Not. eVal(bVldArq,cFile)	//Valido o arquivo por precaução
					cFile	:= ''
				EndIf
			Else
				cFile	:= ''
			EndIf
		EndIf

		If Empty(cFile)
			//Busca pela integração com o SIEG
			cXML	:= ''
			If lRCOMF30
				cXML	:= U_RCOMF30((cAlias)->F1_CHVNFE,(cAlias)->F1_ESPECIE,lMsg)
			EndIf

			If Empty(cXML)
				U_CXMsgErro('Erro ao localizar o arquivo XML do documento.',/*cSolucao*/,lMsg,/*oArea*/)
			EndIf
		Else
			cXML	:= U_CXReadFile(cFile,lMsg)
		EndIf

		oArea:RestArea()		//Restaura area
	Else
		U_CXMsgErro('A espécie '+(cAlias)->F1_ESPECIE+' do documento não possui XML.',;
					/*cSolucao*/,lMsg,/*oArea*/)
	EndIf

Return cXML

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| VisXML                          | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Visualizar o arquivo XML do documento                                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
			aAdd(aArqTmp,cArquivo)	//Guardo os arquivos temporários para posterior limpeza
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
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

	//Apenas estas espécies tem XML
	If AllTrim((cAlias)->F1_ESPECIE) $ 'CTE/SPED'

		oArea		:= tCtrlAlias():GetArea({'SM0'})
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		//Busca no diretório onde os arquivos PDF ficam salvos-----------------------------------------
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
			ApMsgAlert(	'NÃO foi possível encontrar o arquivo PDF salvo na pasta configurada:'+CRLF+;
						cDirPDFCmp+CRLF+CRLF+;
						'SERÁ FEITA UMA TENTATIVA DE IMPRESSÃO A PARTIR DO XML.',U_CXTxtMsg(,,.T.))
			
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
	Else
		U_CXMsgErro('A espécie '+(cAlias)->F1_ESPECIE+' do documento não possui XML.',;
					/*cSolucao*/,/*lMsg*/,/*oArea*/)
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| GravaConf                       | Autor | Cirilo Rocha       | Data | 10/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Rotina para efetuar a gravação da flag de conferência e sua auditoria             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

	//Pré-Validação do registro--------------------------------------------------------------------
	lConferido	:= ( .Not. Empty((cAlias)->F1_YUSRCFG) )
	If 	.Not. lConferido .And. ;
		(cOper==cST_NAOCONFE)
	
		U_CXMsgErro('Status do documento já está NÃO conferido.',;
					/*cSoluc*/,/*lMsg*/,/*oArea*/)
		Return .F.
	ElseIf	lConferido .And. ;
			(cOper==cST_CONFERIDO)

		U_CXMsgErro('Status do documento já está CONFERIDO.',;
					/*cSoluc*/,/*lMsg*/,/*oArea*/)
		Return .F.
	EndIf

	//---------------------------------------------------------------------------------------------
	Begin Transaction
		SF1->(dbGoTo((cAlias)->F1_RECNO))
		If .Not. RecLock('SF1',.F.)	//Tento locar o registro por segurança
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
				U_CXMsgErro('Problema no processamento, status do documento já está NÃO conferido.',;
							'VERIFIQUE SE OUTRO USUÁRIO TAMBÉM ESTÁ TRATANDO ESSE MESMO DOCUMENTO.',/*lMsg*/,/*oArea*/)
				lRet	:= .F.
			ElseIf	lConferido .And. ;
					(cOper==cST_CONFERIDO)

				DisarmTransaction()
				SF1->(MsUnLock())
				U_CXMsgErro('Problema no processamento, status do documento já está CONFERIDO.',;
							'VERIFIQUE SE OUTRO USUÁRIO TAMBÉM ESTÁ TRATANDO ESSE MESMO DOCUMENTO.',/*lMsg*/,/*oArea*/)
				lRet	:= .F.
			EndIf
		EndIf

		If lRet
			If cOper == cST_CONFERIDO
				SF1->F1_YUSRCFG	:= __cUserID+DtoS(Date())
			Else
				SF1->F1_YUSRCFG	:= ''
			EndIf
			
			//Gravação do campo na tabela temporária---------------------------------------------------
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
	
	If FWIsInCallStack('TELACONFERENCIA') //Veio da tela de conferência preciso atualizar o campo F1_YUSRCFG
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
//##|Desc.   | Monta o texto para exibição do usuáio que fez a conferência.                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
//##|Desc.   | Função usada para restrição do menu dos livros fiscais                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
	If SF3->F3_CFO < '5000'				//Por enquanto é apenas para documentos de entrada
		oArea		:= tCtrlAlias():GetArea({'SF1'})
		SF1->(dbSetOrder(1))	//F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_TIPO
		If SF1->(dbSeek(FwXFilial('SF1')+SF3->(F3_NFISCAL+F3_SERIE+F3_CLIEFOR+F3_LOJA)))
			If .Not. Empty(SF1->F1_YUSRCFG)
				U_CXHelp(,,'Este documento já foi marcado como conferido (F1_YUSRCFG):'+CRLF+;
							U_RFISA03A(SF1->F1_YUSRCFG),,;
							'CASO NECESSÁRIO REMOVA O VISTO ANTES DE EFETUAR ALTERAÇÕES/EXCLUSÕES.')
				lProssegue	:= .F.
			EndIf
		Else
			U_CXHelp(,,	'Erro ao localizar o documento de entrada vinculado a este registros:'+CRLF+;
						'Chave: '+FwXFilial('SF1')+'-'+SF3->(F3_NFISCAL+'-'+F3_SERIE+'-'+F3_CLIEFOR+'-'+F3_LOJA),,;
						'INFORME O SETOR DE T.I.')
			lProssegue	:= .F.
		EndIf
		oArea:RestArea()		//Restaura area
	EndIf

	If Type('_aRotMA900') == 'U'	//Variável de controle
		lProssegue	:= .F.
		U_CXHelp(,,	'Erro interno, variável _aRotMA900 não encontrada.'+CRLF+;
					'Deveria ser gerada no P.E. MA900MNU.',,;
					'INFORME O SETOR DE T.I.')
	ElseIf nOpc == NIL
		lProssegue	:= .F.
		U_CXHelp(,,	'Erro interno, variável nOpc não foi recebida.',,;
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
//##|        | Feito de forma segregada porque o padrão não consegui filtrar a consulta direta-  |##
//##|        |  mente, o alias fica setado na Z05 e não na tabela do browse                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| ObsSC7                          | Autor | Cirilo Rocha       | Data | 14/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Visualiza observações do pedido de compras SC7                                    |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
		U_CXMsgErro('Este documento NÃO está vinculado a um Pedido de Compras (D1_PEDIDO).',;
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
	Else
		If FWIsInCallStack('TELACONFERENCIA')	//Veio da tela de conferência
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
		ApMsgInfo(	'Nenhuma observação.',U_CXTxtMsg(,,.T.))
	Else
		u_CXApMsgMemo(cMmObs,U_CXTxtMsg()+"Observações",.F.)
	EndIf

Return

//#############################################################################
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Programa  | ModelDef | Autor | Cirilo Rocha       | Data | 25/10/2022  |##
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Descr.    | Função para montar as estrutura de dados e as regras       |##
//##+----------+----------+-------------------------------------------------+##
//##| DATA     | ANALISTA | MANUTENÇÃO EFETUADA                             |##
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

	// Construção de uma estrutura de dados--------------------------------------------------------
	oStruSF1 := FwFormStruct( nTP_MODEL, 'SF1' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ ) 
	oStruSA2 := FwFormStruct( nTP_MODEL, 'SA2' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )
	oStruSD1 := FwFormStruct( nTP_MODEL, 'SD1' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )
	oStruSFT := FwFormStruct( nTP_MODEL, 'SFT' , {|cCpo| FiltraCpo(cCpo) },/*lViewUsado*/ )

	//Ajusto manualmente o tamanho destes campos para exibição
	oStruSF1:SetProperty('F1_YUSRLAN' 	, MVC_MODEL_TAMANHO,35)
	oStruSF1:SetProperty('F1_YUSRCFG' 	, MVC_MODEL_TAMANHO,35)
	oStruSD1:SetProperty('D1_COD'	 	, MVC_MODEL_TAMANHO,35)

	//Desmarco os campos obrigatórios, é somente visualização NÃO precisa
	oStruSF1:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)
	oStruSA2:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)
	oStruSFT:SetProperty('*' 			, MVC_MODEL_OBRIGAT,.F.)

	oStruSF1:AddField(	'Natureza Financeira',;	//[01] C Titulo do campo (X3Titulo())
						'Natureza Financeira',;	//[02] C ToolTip do campo (X3Descric())
						'F1_NATUREZ'		,;	//[03] C identificador (ID) do Field (X3_CAMPO)
						'C'					,;	//[04] C Tipo do campo (X3_TIPO)
						35					,;	//[05] N Tamanho do campo (X3_TAMANHO)
						0					,;	//[06] N Decimal do campo (X3_DECIMAL)
						/*bValid*/			,;	//[07] B Code-block de validação do campo (B.E. X3_VALID)
						/*bWhen*/			,;	//[08] B Code-block de validação When do campo (B.E. X3_WHEN)
						/*aValues*/			,;	//[09] A Lista de valores permitido do campo (X3CBox())
						/*lObrigat*/		,;	//[10] L Indica se o campo tem preenchimento obrigatório (X3Obrigat())
						/*bInit*/			,;	//[11] B Code-block de inicializacao do campo (B.E. X3_RELACAO)
						.F.					,;	//[12] L Indica se trata de um campo chave
						/*lNoUpd*/			,;	//[13] L Indica se o campo pode receber valor em uma operação de update.
						.T.					,;	//[14] L Indica se o campo é virtual (X3_CONTEXT=V)
						/*cValid*/			)	//[15] C Valid do usuário em formato texto e sem alteração, usado para se criar o aHeader de compatibilidade

	oModel   := MpFormModel():New(	'RFISA03M'		,;	//01 cID	//Não pode ter o mesmo nome do fonte
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

	// Adiciona a Descrição do Componente do Modelo de Dados
	oModel:SetDescription('Conferência Fiscal')

	// Adiciona a descrição dos Componentes do Modelo de Dados
	oModel:GetModel( 'MODEL_SF1' ):SetDescription( 'Cabeçalho (SF1)' )

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
	 				/*bLinhaOK*/	,;	//05 Validação equivale ao LinhaOK
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
	//oModel:SetRelation('MODEL_SFT', {;	//NÃO É NECESSÁRIO PORQUE JÁ ESTOU USANDO UM BLOCO DE LEITURA CUSTOMIZADO
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
//	 				/*bLinhaOK*/	,;	//05 Validação equivale ao LinhaOK
//	 				/*bPre*/		,;	//06 bPre
//	 				/*bPost*/ 		,;	//07 bPost
//	 				{|oFieldModel, lCopy| loadField(oFieldModel,lCopy,.T.) } ) 	//06 bLoad

Return( oModel )

//#############################################################################
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Programa  | ViewDef  | Autor | Cirilo Rocha       | Data | 25/10/2022  |##
//##+----------+----------+-------+--------------------+------+-------------+##
//##|Descr.    | Função para montar a interface da tela                     |##
//##+----------+----------+-------------------------------------------------+##
//##| DATA     | ANALISTA | MANUTENÇÃO EFETUADA                             |##
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

	//Inicialização de Variáveis-------------------------------------------------------------------
	oModel    	:= FWLoadModel('RFISA03')	//Nome do fonte onde está o modelo

	oView		:= FWFormView():New()

	oView:SetModel( oModel )
//	oView:SetContinuousForm()

	oView:addUserButton('Observações (F3)'		,;	//01 cTitle
						'CLIPS'					,;	//02 cResource
						{|| TelaObs()}			,;	//03 bBloco
						'Observações (F3)'		,;	//04 cToolTip
						/*VK_F3*/				,;	//05 nShortCut
						{MODEL_OPERATION_VIEW}	,;	//06 aOptions
						.F.						)	//07 lShowBar

	//Adiciono botões customizados na view---------------------------------------------------------
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

	oView:addUserButton('Próximo Doc. (F12) >>'	,;	//01 cTitle
						'CLIPS'					,;	//02 cResource
						{|| ProximoDoc()}		,;	//03 bBloco
						'Próximo Doc. (F12) >>'	,;	//04 cToolTip
						/*VK_F12*/				,;	//05 nShortCut
						{MODEL_OPERATION_VIEW}	,;	//06 aOptions
						.T.						)	//07 lShowBar

	// Construção de uma estrutura de dados
	oStruSF1 := FwFormStruct( nTP_VIEW, 'SF1' ,{|cCpo| FiltraCpo(cCpo) }) 
	oStruSA2 := FwFormStruct( nTP_VIEW, 'SA2' ,{|cCpo| FiltraCpo(cCpo) })
	oStruSD1 := FwFormStruct( nTP_VIEW, 'SD1' ,{|cCpo| FiltraCpo(cCpo) })
	oStruSFT := FwFormStruct( nTP_VIEW, 'SFT' ,{|cCpo| FiltraCpo(cCpo) })

	//Coloca as descrições completas nos campos
	U_CXDescMVC(oStruSF1,.T.)
	U_CXDescMVC(oStruSA2,.T.)
	U_CXDescMVC(oStruSD1,.F.)
	U_CXDescMVC(oStruSFT,.F.)

	//Para ganhar um pouco de espaço, estou colocando no nome do fornecedor
	oStruSF1:RemoveField('F1_FORNECE')	//Remove campo da estrutura
	oStruSF1:RemoveField('F1_LOJA')		//Remove campo da estrutura

	//Monta o combo do tipo de NF, no padrão não existe combo!
	oStruSF1:SetProperty('F1_TIPO' , MVC_VIEW_COMBOBOX,U_CXTipoNF(4))
	//Uso a descrição resumida mesmo, a completa é muito ruim
	oStruSF1:SetProperty('F1_TIPO' , MVC_VIEW_TITULO,FwX3Titulo('F1_TIPO'))
	
	oStruSF1:AddField(	'F1_NATUREZ'		,;  //[01] C Nome do Campo (X3_CAMPO)
						'07'        		,;  //[02] C Ordem (X3_ORDEM)
						'Natureza Financeira',;  //[03] C Titulo do campo (X3Titulo())
						'Natureza Financeira',;  //[04] C Descrição do campo (X3Descric())
						/*aHelp*/			,;  //[05] A Array com Help
						'GET'		   		,;  //[06] C Tipo do campo (**)
						'@!'        		,;  //[07] C Picture (X3_PICTURE)
						/*bPicVar*/ 		,;  //[08] B Bloco de Picture Var (B.E. X3_PICTVAR)
						/*cF3*/        		,;  //[09] C Consulta F3 (X3_F3)
						.F.         		,;  //[10] L Indica se o campo é editável (X3_VISUAL<>V)
						/*cFolder*/    		,;  //[11] C Pasta do campo (X3_FOLDER)
						/*cAgrupa*/    		,;  //[12] C Agrupamento do campo (X3_AGRUP)
						/*aCombo*/  		,;  //[13] A Lista de valores permitido do campo (X3_COMBO) 
						/*nMaxComb*/  		,;  //[14] N Tamanho Maximo da maior opção do combo
						/*cInicBrw*/  		,;  //[15] C Inicializador de Browse (X3_INIBRW)
						.T.         		,;  //[16] L Indica se o campo é virtual (X3_CONTEXT=V)
						/*cPictVar*/   		)   //[17] C Picture Variável

	oStruSFT:SetProperty('FT_BASEPIS', MVC_VIEW_TITULO,'Base de Cálculo Pis/Cofins')
	oStruSFT:SetProperty('FT_CSTPIS', MVC_VIEW_TITULO,'Cod.Sit.Trib. Pis/Cofins')
	
	oStruSD1:SetProperty('D1_TES'	, MVC_VIEW_TITULO,'TES')
	oStruSD1:SetProperty('D1_CF'	, MVC_VIEW_TITULO,'CFOP')
	oStruSD1:SetProperty('D1_ITEM'	, MVC_VIEW_TITULO,'Item')
	oStruSD1:SetProperty('D1_PEDIDO', MVC_VIEW_TITULO,'Pedido')
	oStruSD1:SetProperty('D1_ITEMPC', MVC_VIEW_TITULO,'It PC')

	//Para não ocorrer erros na abertura da tela com a função padrão!
	oStruSA2:SetProperty('A2_CGC' , MVC_VIEW_PVAR,FwBuildFeature(STRUCT_FEATURE_PICTVAR , 'U_CXFJ()'))
	//Retiro os folders
	oStruSA2:SetProperty('*', MVC_VIEW_FOLDER_NUMBER,"")
	oStruSA2:aFolders	:= {}

	oStruSA2:SetProperty('A2_NOME'	, MVC_VIEW_TITULO,'Código-Loja-Nome ou Razão Social Fornecedor/Cliente')
	oStruSA2:SetProperty('A2_CGC'	, MVC_VIEW_TITULO,'CNPJ/CPF do Fornecedor/Cliente')

	oView:AddField( 'VIEW_SF1'	, oStruSF1	, 'MODEL_SF1' )	//Cabeçalho docto (SF1)
	oView:AddField( 'VIEW_SA2'	, oStruSA2	, 'MODEL_SA2' )	//Fornecedor/Cliente (SA2/SA1)
	oView:AddGrid(	'VIEW_SD1'	, oStruSD1	, 'MODEL_SD1' )	//Itens Docto (SD1)
	//oView:AddGrid(	'VIEW_SFT'	, oStruSFT	, 'MODEL_SFT' )	//Livros Fiscais (SFT)
	oView:AddField(	'VIEW_SFT'	, oStruSFT	, 'MODEL_SFT' )	//Livros Fiscais (SFT)


	//Painel Superior------------------------------------------------------------------------------
	oView:CreateVerticalBox(  'PAINEL_PRINCIPAL', 100 )

		oView:EnableTitleView('VIEW_SF1','Cabeçalho (SF1)')
		oView:CreateHorizontalBox( 'CABECALHO' ,  40, 'PAINEL_PRINCIPAL' )
		oView:SetOwnerView( 'VIEW_SF1' 	, 'CABECALHO')	//Relaciona o identificador (ID) da View com o 'box' para exibição
		
		oView:EnableTitleView('VIEW_SA2','Fornecedor (SA2) / Cliente (SA1)')
		oView:CreateHorizontalBox( 'FOLDER_SA2', 15, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SA2' 	, 'FOLDER_SA2' )	//Relaciona o identificador (ID) da View com o 'box' para exibição
		
		oView:EnableTitleView('VIEW_SD1',oModel:GetModel( 'MODEL_SD1' ):GetDescription())
		oView:CreateHorizontalBox( 'GRID_SD1', 25, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SD1' 	, 'GRID_SD1' )	//Relaciona o identificador (ID) da View com o 'box' para exibição

		oView:EnableTitleView('VIEW_SFT','Livros Fiscais (SFT)')
		oView:CreateHorizontalBox( 'FOLDER_SFT', 20, 'PAINEL_PRINCIPAL',,/*cIDFolder*/,/*cIDSheet*/ )
		oView:SetOwnerView( 'VIEW_SFT' 	, 'FOLDER_SFT' )	//Relaciona o identificador (ID) da View com o 'box' para exibição

	//Fim PAINEL_PRINCIPAL

Return( oView )

//-------------------------------------------------------------------------------------------------
Static Function FiltraCpo(cCpo)

Return aScan(aCampos,{|x| x == RTrim(cCpo)}) > 0

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| loadField                       | Autor | Cirilo Rocha       | Data | 22/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Função responsável pela carga dos dados para o modelo MVC                         |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
//##|Desc.   | Função auxiliar para carregar os dados dos modelos MVC                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
		U_CXHelp(,,'Chegou ao final da relação de documentos. NÃO é possível avançar.',,/*cSolucao*/,.T.)
	Else
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		oModelCache:DeActivate()	//Força a recarga dos dados
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
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
		U_CXHelp(,,'Chegou ao início da relação de documentos. NÃO é possível voltar mais.',,/*cSolucao*/,.T.)
	Else
		If .Not. ( cFilAnt == (cAlias)->F1_FILIAL )
			cFilAnt	:= (cAlias)->F1_FILIAL
			FWSM0Util():setSM0PositionBycFilAnt()
		EndIf

		oModelCache:DeActivate()	//Força a recarga dos dados
		oModelCache:Activate()
		AtualizaTelaMVC()
	EndIf

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| TelaObs                         | Autor | Cirilo Rocha       | Data | 24/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Tela auxiliar para conseguir mostrar as observações dentro da tela MVC, porque nos|##
//##|        |  botões do MVC não existe suporte a sub-menus.                                    |##
//##|        | http://devforum.totvs.com.br/3405-adicionar-submenu-view-mvc                      |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
	
	oDlg	:= MSDialog():New(	000,000,nAltTela,(nLarBt*3)+010,U_CXTxtMsg()+"Observações PC/SC",;
								/*cPar6*/,/*nPar7*/,/*lPar8*/,/*DS_MODALFRAME*/,/*anClrText*/,/*anClrBack*/,;
								/*oPar12*/,/*oWnd*/,.T.,,,,/*lTransparent*/)
		
		aPosBt	:= U_CXPosBtn(oDlg,nLarBt,nAltBt)

		For nX := 1 to Len(aMenuObs)
			TButton():New( 	005+(nX-1)*(nAltBt+005), 025, aMenuObs[nX][cRX_DESC], oDlg,aMenuObs[nX][bRX_EXEC],;
							nLarBt, nAltBt,,,.F.,.T.,.F.,,.F.,,,.F. )
		Next
		 
		TButton():New( aPosBt[1], 025, "&Fechar", oDlg,{||  oDlg:end()}, nLarBt, nAltBt,,,.F.,.T.,.F.,,.F.,,,.F. )

	// ativa diálogo centralizado
	oDlg:Activate(/*uPar1*/,/*uPar2*/,/*uPar3*/,.T./*lCenter*/,/*{|Self| Valid }*/,/*uPar6*/,/*{|Self| Init }*/ )

	lTelaObs	:= .F.

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| AtualizaTelaMVC                 | Autor | Cirilo Rocha       | Data | 27/02/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Força a atualização do objeto MVC, quando estava chamando diretamente pelo atalho |##
//##|        |  F# o sistema não estava atualizando.                                             |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
//##|Desc.   | Chama a tela padrão de Conhecimento (MPDocument)                                  |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
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
	aAdd(aRotina,{,,,nOPC_VISUAL})	//Para forçar ficar somente visualização!

	MPDocument('SF1',SF1->(Recno()),1)

Return

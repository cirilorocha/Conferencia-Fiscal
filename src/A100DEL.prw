#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'
#INCLUDE 'CXInclude.ch'

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| A100DEL                         | Autor | Cirilo Rocha       | Data | 08/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Descr.  | Ponto de entrada na exclusão dos documentos de entrada, usado para validar as in- |##
//##|        |  formações fiscais do documento                                                   |##
//##|        |                                                                                   |##
//##|        | Foi feito assim, pois, o P.E. MT100TOK não é chamado na exclusão                  |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|xx/xx/xx|          | USADO PARA NÃO PERMITIR EXCLUIR OU ESTORNAR CLASSIFICAÇÃO DE DOCUMENTOS|##
//##|        |          |  QUE JÁ FORAM CONFERIDOS PELA ROTINA RFISA03                           |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
User Function A100DEL();
				AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lRet	:= .T.			AS Logical

	//---------------------------------------------------------------------------------------------
	If lRet
		If .Not. Empty(SF1->F1_YUSRCFG)
			lRet	:= .F.

			U_CXMsgErro('Não é permitida a exclusão de documentos conferidos pelo Setor Fiscal.'+CRLF+;
						'Conferido: '+U_RFISA03A(SF1->F1_YUSRCFG),;
						'SE NECESSÁRIO ENTRE EM CONTATO COM O SETOR FISCAL PARA RETIRAR O VISTO.',/*lMsg*/,/*oArea*/)
		EndIf
	EndIf

	//Usuarios da TI-------------------------------------------------------------------------------
	If 	.Not. lRet .And. ;
		.Not. U_CXRotAuto() .And. ;
		U_RGENF01(,.F.,.T.)

		If ApMsgNoYes('<b>ADMIN:</b> Tem certeza que deseja prosseguir ?',U_CXTxtMsg(,,.T.))
			lRet	:= .T.
		EndIf
	EndIf

Return lRet

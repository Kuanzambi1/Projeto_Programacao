	fFicheiro file
	literal filePath <- "C:\\ProjectoAlgoritmo\\BDClientesTT.txt"
	logico ficheiroExistente <- fExiste(filepath)
	
registo Conta
	inteiro numero_de_conta,tipo_de_conta,codigo_cliente
	real saldo
	inteiro estado
fimRegisto

registo Pessoa
	literal nome,localidade,bi
	inteiro telefone,codigo_cliente
fimRegisto

  	Pessoa cliente[100]
  	Conta contas[100]
  	inteiro tipo_de_conta, qtdContas <-0,n,codigo
  	inteiro qtdCliente <- 0
  	inteiro contLinha <- 0

  	procedimento continuar()
  		literal q 
  		escreva"Digite qualquer tecla para continuar:"
  		leia q
  	fimProcedimento

  	//Cadastro de clientes
	funcao logico cadastro_de_cliente (Pessoa p)
	
		cliente[qtdCliente].nome <- p.nome
		cliente[qtdCliente].codigo_cliente <- qtdCliente + 1
		cliente[qtdCliente].bi <- p.bi
		cliente[qtdCliente].telefone <- p.telefone
		cliente[qtdCliente].localidade <- p.localidade
		qtdCliente <- qtdCliente + 1

		retorne verdadeiro
	fimFuncao

	funcao inteiro buscar_indice(inteiro codigo)
		inteiro i
		para i de 0 ate qtdCliente - 1
			se codigo = cliente[i].codigo_cliente entao
				retorne i
			fimse
		fimPara
			retorne -1
	fimFuncao

	funcao inteiro verificar_numero_de_conta(inteiro num_conta)
		inteiro i
		para i de 0 ate qtdCliente - 1
			se num_conta = contas[i].numero_de_conta entao
				retorne i
			fimse
		fimPara 
		retorne -1
	fimFuncao

funcao real consultar_saldo (inteiro num_da_conta)
	inteiro num	
	num <- verificar_numero_de_conta(num_da_conta)
	se num <> -1 entao
		retorne contas[num].saldo
	fimSe
		retorne -1
fimfuncao
	
	//Salvar os dados no Ficheiro
	procedimento copia_de_seguranca()
		inteiro i
  		literal cabecalho <-"Codigo\tNome\tBi\tTelefone\tlocalidade\n"

  		fAbrir(file, filePath, verdadeiro)
  		
  		para i de 0 ate qtdCliente - 1
			fescrever( file , strIntParaStr(cliente[i].codigo_cliente) + "\t"+ cliente[i].nome+"\t"+ cliente[i].bi+"\t"+strIntParaStr(cliente[i].telefone)+"\t"+cliente[i].localidade + "\t")
			inteiro indice <- buscar_indice(cliente[i].codigo_cliente) 
			se indice <> -1 entao
				
				fEscrever(file,strIntParaStr(contas[i].codigo_cliente)+"\t"+strIntParaStr(contas[i].tipo_de_conta)+ "\t"+ strIntParaStr(contas[i].numero_de_conta)+"\t"+strRealParaStr(contas[i].saldo)+"\t"+ strIntParaStr(contas[i].estado))
			fimSe
			fEscrever(file, "\n")
		fimPara

		fFechar(file)
	fimProcedimento

	//abertura de conta
	funcao logico abertura_de_conta(PESSOA p, inteiro tipo_de_conta)
			
			contas[qtdContas].codigo_cliente <- cliente[qtdContas].codigo_cliente
			contas[qtdContas].tipo_de_conta <- tipo_de_conta			
			contas[qtdContas].numero_de_conta <- qtdContas + 1000
			contas[qtdContas].saldo <- 0
			contas[qtdContas].estado <- 1

			qtdContas <- qtdContas + 1

		retorne verdadeiro
	fimFuncao //fim de abertura de conta
	
	procedimento listar_clientes()
		inteiro i
		para i de 0 ate qtdCliente - 1
			mostre "Nome: ",cliente[i].nome,"\tCódigo Cliente: ",cliente[i].codigo_cliente,"\tBI/NIF: ",cliente[i].bi,"\tLocalidade: ",cliente[i].localidade,"\n"
			mostre "Código conta: ",contas[i].codigo_cliente,"\tTipo de conta: ",contas[i].tipo_de_conta,"\tNº de conta: ",contas[i].numero_de_conta,"\tEstado: ",contas[i].estado,"\n"
			escreva "___________________________________________________________________________________\n"
		fimPara
		
	fimprocedimento

	//deposito
	funcao logico depositar_dinheiro(inteiro num_da_conta, real valor)
		inteiro num
		
		num <- verificar_numero_de_conta(num_da_conta)
			se num <> -1 e valor >= 10 e valor < 12500000  entao
				contas[num].saldo <- valor + contas[num].saldo
			 retorne verdadeiro
			fimSe
		retorne falso
	fimFuncao 
	//fim deposito

	funcao inteiro buscar_indiceconta(inteiro codigo)
		inteiro i
		para i de 0 ate qtdCliente - 1
			se codigo = contas[i].numero_de_conta  entao
				retorne i
			fimse
		fimPara
			retorne -1
	fimFuncao

 //levantar valores

 	funcao logico Levantar_dinheiro(inteiro num_da_conta, real valor)
		inteiro num
		num <- verificar_numero_de_conta(num_da_conta)
			se num<> -1 e valor<=contas[num].saldo e valor <=10000000 entao
			contas[num].saldo<- contas[num].saldo - valor
			
				retorne verdadeiro
	       fimse
				
		retorne falso
	fimfuncao
     //transferir
	funcao logico transferir_dinheiro (inteiro num_da_conta_enviar,inteiro num_da_conta_receber, real valor)
		inteiro posicao_enviar, posicao_receber
		
		   posicao_enviar  <- verificar_numero_de_conta(num_da_conta_enviar)
		   posicao_receber <- verificar_numero_de_conta(num_da_conta_receber)

		   se posicao_receber <> -1 e posicao_enviar <> -1 entao
		      conta c_envio <- contas[posicao_enviar]
		      conta c_receber <- contas[posicao_receber] 
		   
			se  valor >= 10 e valor < 12500000 e c_envio.saldo >= valor  entao
				
				c_envio.saldo <- c_envio.saldo - valor 
				c_receber.saldo <- c_receber.saldo + valor
				 
			 retorne verdadeiro
			fimSe
		fimSe
	retorne falso
	fimFuncao 

	//Atualisar dados
	funcao logico actualizar_dados (Pessoa p, inteiro codigo_cliente)
		inteiro indice <- buscar_indice(codigo_cliente)
		cliente[indice].nome <- p.nome
		cliente[indice].bi <- p.bi
		cliente[indice].telefone <- p.telefone
		cliente[indice].localidade <- p.localidade

		retorne verdadeiro
	fimFuncao

	procedimento restaurar_copia_de_seguranca()
		fFicheiro ficheiro
		fAbrir(ficheiro, filePath, falso)

		enquanto fTemLinha(ficheiro) faca
			literal getValue[] <- strRepartir(fLerLinha(ficheiro),"\t")
			 cliente[qtdCliente].codigo_cliente <- strValorInt(getValue[0])
			 cliente[qtdCliente].nome <- getValue[1]
			 cliente[qtdCliente].bi <- getValue[2]
			 cliente[qtdCliente].telefone <- strValorInt(getValue[3])
			 cliente[qtdCliente].localidade <- getValue[4]
			 
			contas[qtdContas].codigo_cliente <- strValorInt(getValue[5])
			contas[qtdContas].tipo_de_conta <- strValorInt(getValue[6])			
			contas[qtdContas].numero_de_conta <- strValorInt(getValue[7])
			contas[qtdContas].saldo <- strValorReal(strTrocar(getValue[8], ",", ""))
			contas[qtdContas].estado <- strValorInt(getValue[9])
			 
			qtdCliente<- qtdCliente + 1
			qtdContas <- qtdContas + 1
	    fimenquanto
	    fFechar(ficheiro)
	fimprocedimento

inicio
	restaurar_copia_de_seguranca()

	inteiro opcao,i, tipo_de_conta,codigo,indice
	Pessoa p
	Conta c

	se nao ficheiroExistente entao
		se nao fCriar(filepath)	entao
			mostre"Nao foi possivel criar o ficheiro"
			fimAlgoritmo
		fimse
	fimse

	enquanto opcao <> 9 faca
		limpatela
		escreva"MENU"
		escreva"\n1: Cadastrar Cliente \n2: Listar Clientes \n3: Abertura de Conta \n4: Depositar Dinheiro \n5: Levantar Dinheiro \n6: Consultar Saldo \n7: Transferir Dinheiro \n8: Actualizar Dados do Cliente \n9: Terminar\n"
		leia opcao
	
		escolha opcao
		caso 1:
			limpatela
			escreva"Insira o nome:"
			leia p.nome
			escreva"Insira o número do BI/NIF:"
			leia p.bi
			escreva"Insira o seu contacto telefónico:"
			leia p.telefone
			escreva"Insira a sua localidade: "
			leia p.localidade
			se cadastro_de_cliente(p) = verdadeiro entao
				escreva"Cadastrado com sucesso\n"
			fimse
	   		
	   		continuar()
		caso 2:
			limpatela
			listar_clientes()
			continuar()
		caso 3:
			limpatela
			escreva"Digite \n0- Caso seja conta pessoal \n1- Caso seja conta empresarial\n:"
			leia tipo_de_conta
			escreva"Insira o codigo: "
			leia codigo

			indice <- buscar_indice(codigo)
			
			se indice <> -1 entao
				abertura_de_conta(cliente[i],tipo_de_conta)
				escreva"Conta aberta com sucesso\n"
			senao
				escreva"Código inexistente\n"
			fimse
			continuar()
		caso 4:
			limpatela
			inteiro numero_de_conta
			real valor
			logico resultado
			
			escreva"Insira o numero de conta: "
			leia numero_de_conta

			escreva"Insira o valor a depositar: "
			leia valor
			
			resultado <- depositar_dinheiro(numero_de_conta, valor)
               se resultado entao 
			   
			escreva "O valor, Kz ",valor," foi depositado com sucesso \n"
	
               senao 
               	escreva "Falha na operação\n"
        fimse
		
			continuar()
		caso 5:
			/*Levantar dinheiro*/
			Limpatela 
				inteiro num_da_conta
			real valor
			logico demostracao
			mostre "Digite o numero de conta: "
			leia num_da_conta
				mostre "Digite o valor: "
			leia valor
			
	      demostracao<-Levantar_dinheiro( num_da_conta, valor)
			se demostracao entao
				escreva "O valor kz " , valor , " foi levantado com sucesso\n"
			senao
				escreva "Não é possivel levantar" ,"\n"
			fimse
			continuar()
		caso 6:
			/*Consultar saldo*/
			limpatela
		
				escreva "Insira o número de conta:"
			leia c.numero_de_conta
               real saldo <- consultar_saldo(c.numero_de_conta)
               se saldo <> -1 entao 
			escreva "Saldo atual é de kz ",saldo ,  "\n"
               senao
               	escreva "Conta inexistente\n"
			fimse
			continuar()

		caso 7:
			/*Transferir saldo*/
			limpatela
			inteiro num_c_enviar, num_c_receber 
			real valor
			logico resultado
		   escreva "Insira o seu número de conta  "
			 leia num_c_enviar
		   escreva "Insira o número de conta a receber a transferencia "
		     leia num_c_receber
		   escreva "Insira o valor a transferir  "
		     leia valor 
		     
		    resultado <- transferir_dinheiro ( num_c_enviar ,num_c_receber  ,valor )
		  se resultado entao
		  	escreva "Transferencia realizada com sucesso\n "
		  senao 
		  	escreva "Falha na transferencia\n"
		  fimse
		 
            continuar()
		caso 8:
			/*Actualizar dados*/
			limpatela
			escreva "Insira o código do cliente: "
			leia codigo

			se buscar_indice(codigo) <> -1 entao
				escreva"Insira o nome:"
				leia p.nome
				escreva"Insira o número do BI/NIF:"
				leia p.bi
				escreva"Insira o seu contacto telefónico:"
				leia p.telefone
				escreva"Insira a sua localidade: "
				leia p.localidade
				se actualizar_dados (p,codigo) = verdadeiro entao
					Escreva"Dados actualisados com sucesso\n"
				fimse
			senao
				escreva"Cliente inexistente\n"
			fimSe
		
		fimEscolha
			
	fimEnquanto
	
	copia_de_seguranca()
fimAlgoritmo
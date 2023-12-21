import analisadorlexsint
import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from tkinter import messagebox

def analisar_codigo():
    codigo = editor_texto.get("1.0", "end-1c")
    tokens = analisadorlexsint.test_lexer(codigo)
    error_message = analisadorlexsint.test_parser(codigo)
    exibir_tokens(tokens, error_message)

def exibir_tokens(tokens, error_message):
    editor_texto.tag_remove('erro', '1.0', 'end')
    janela_tokens = tk.Toplevel()
    janela_tokens.title("Tokens")

    frame_tokens = tk.Frame(janela_tokens)
    frame_tokens.pack(fill="both", expand=True)

    if(error_message):
        destacar_erros(error_message)
        messagebox.showerror("Error", error_message)

    tabela = ttk.Treeview(frame_tokens, columns=('Token', 'Linha'), show='headings')
    tabela.heading('Token', text='Token')
    tabela.heading('Linha', text='Linha')
    for token, linha in tokens:
        tabela.insert('', 'end', values=(token, linha))
    tabela.pack(expand=True, fill='both')

def destacar_erros(erro):
    palavras = erro.split()
    linha = palavras[-1]
    editor_texto.tag_configure('erro', background='red')
    inicio = f"{linha}.0"
    fim = f"{linha}.end"
    editor_texto.tag_add('erro', inicio, fim)

def atualizar_linhas(event=None):
    linhas = ''
    linha_atual = int(editor_texto.index('insert').split('.')[0])
    for i in range(1, linha_atual + 1):
        linhas += str(i) + '\n'
    numeros_linha.config(state='normal')
    numeros_linha.delete('1.0', 'end')
    numeros_linha.insert('1.0', linhas)
    numeros_linha.config(state='disabled')
    
def sincronizar_rolagem(*args):
    numeros_linha.yview(*args)
    editor_texto.yview(*args)

def carregar_arquivo():
    caminho_arquivo = filedialog.askopenfilename(filetypes=[("Text files", "*.txt")])
    if caminho_arquivo:
        with open(caminho_arquivo, 'r') as arquivo:
            editor_texto.delete('1.0', tk.END)
            editor_texto.insert('1.0', arquivo.read())
        atualizar_linhas() 

def salvar_arquivo():
    caminho_arquivo = filedialog.asksaveasfilename(defaultextension=".txt")
    if caminho_arquivo:
        with open(caminho_arquivo, 'w') as arquivo:
            arquivo.write(editor_texto.get('1.0', tk.END))

app = tk.Tk()
app.title("Compilador MINI Java")

# Menu
menu_principal = tk.Menu(app)
app.config(menu=menu_principal)

# Menu Arquivo
menu_arquivo = tk.Menu(menu_principal, tearoff=0)
menu_principal.add_cascade(label="Arquivo", menu=menu_arquivo)
menu_arquivo.add_command(label="Carregar", command=carregar_arquivo)
menu_arquivo.add_command(label="Salvar", command=salvar_arquivo)

frame_editor = tk.Frame(app)
frame_editor.pack(fill="both", expand=True)

numeros_linha = tk.Text(frame_editor, width=4, state='disabled')
numeros_linha.pack(side="left", fill="y")

scrollbar = tk.Scrollbar(frame_editor, command=sincronizar_rolagem)

# Habilitar desfazer e configurar a rolagem
editor_texto = tk.Text(frame_editor, undo=True, yscrollcommand=scrollbar.set)

scrollbar.pack(side="right", fill="y")
editor_texto.pack(side="right", fill="both", expand=True)
numeros_linha.config(yscrollcommand=scrollbar.set)

# Atalho de teclado para desfazer (Ctrl+Z)
editor_texto.bind('<Control-z>', lambda event: editor_texto.edit_undo())

editor_texto.bind('<KeyPress>', lambda event: app.after(1, atualizar_linhas))
editor_texto.bind('<MouseWheel>', atualizar_linhas)
botao_analisar = tk.Button(app, text="Compilar código", command=analisar_codigo)
botao_analisar.pack()

atualizar_linhas()
app.mainloop()
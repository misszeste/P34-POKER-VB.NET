
Public Class Form1

    Inherits System.Windows.Forms.Form

    Public paquetDeCartes(51) As String
    Public gain() As Integer = {1, 3, 5, 10, 15, 20, 25, 50, 100}
    Public mainJoueur1(4) As Integer
    Public mainJoueur2(4) As Integer
    Public mainFinal(4) As Integer
    Public debutJeu As Boolean = False
    Public deuxiemeMain As Boolean = True
    Public compteurDeChiffre(12) As Integer
    Public compteurDeLettre(3) As Integer
    Public continuer As Boolean


    Private Sub Form1_Load(ByVal sender As System.Object,
        ByVal e As System.EventArgs) Handles MyBase.Load
        MsgBox("Bienvenue sur mon application Poker ") 'affiche msg avant d'ouvrir l'app'

        Dim reponse As String
        Do
            reponse = InputBox(" Quel est votre nom de joueur?")
            If String.ReferenceEquals(reponse, String.Empty) Then 'validation btn cancel 
                MsgBox("Écrit un nom!")
            Else
                Label1.Text = "Bonjour " & reponse
            End If
        Loop While reponse = Nothing

        Do
            reponse = InputBox(" Entrez le montant de votre crédit : ")
            creditBox.Text = reponse
            If IsNumeric(reponse) = False Then
                MsgBox("Rentre donc un chiffre  !!")
            End If
        Loop While IsNumeric(reponse) = False

        Dim k As Integer = 0
        For j As Integer = 0 To 3
            For i As Integer = 1 To 13
                ' coeur pour le 0 
                If (j = 0) Then
                    paquetDeCartes(k) = ((i) & "h.gif")
                End If

                If (j = 1) Then
                    paquetDeCartes(k) = ((i) & "c.gif")
                End If

                If (j = 2) Then
                    paquetDeCartes(k) = ((i) & "s.gif")
                End If

                If (j = 3) Then
                    paquetDeCartes(k) = ((i) & "d.gif")
                End If
                k = k + 1
            Next
        Next
    End Sub


    Private Sub BtnJouer_Click(sender As Object, e As EventArgs) Handles BtnJouer.Click
        If debutJeu = False Then
            debutJeu = True
            continuer = True
            CheckBox1.Enabled = True
            CheckBox2.Enabled = True
            CheckBox3.Enabled = True
            CheckBox4.Enabled = True
            CheckBox5.Enabled = True

            If creditBox.Text = 0 Then
                MsgBox("Aucun crédit, partie terminée!")
                continuer = False
                debutJeu = False
            End If

            If creditBox.Text - miseBox.Text < 0 And continuer = True Then
                MsgBox("Pas assez de crédit pour jouer! Baisse ta mise !")
                continuer = False
                debutJeu = False
            End If

            If continuer = True Then
                creditBox.Text = creditBox.Text - miseBox.Text
                ReDim mainJoueur1(4)
                For i As Integer = 0 To 4
                    'Initialiser le random-number generator.
                    Randomize()
                    Dim value As Integer
                    Dim trouve As Boolean
                    Do
                        trouve = True
                        ' Generer random value between 0 and 51
                        value = Int(Int((51 * Rnd()) + 0))
                        For x As Integer = 0 To 4
                            If mainJoueur1(x) = value Then
                                trouve = False 'numero existant dans la combinaison
                            End If
                        Next
                    Loop While trouve = False
                    mainJoueur1(i) = value
                    Select Case i ' afficher 1er main
                        Case 0
                            CarteBox1.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                        Case 1
                            CarteBox2.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                        Case 2
                            CarteBox3.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                        Case 3
                            CarteBox4.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                        Case 4
                            CarteBox5.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                    End Select
                Next
            End If
        Else
            MsgBox("Veuillez cliquer sur CONTINUER")
        End If
    End Sub


    Private Sub BtnContinuer_Click(sender As Object, e As EventArgs) Handles BtnContinuer.Click
        If debutJeu = True Then
            Dim reponse As Integer
            If deuxiemeMain = True Then
                deuxiemeMain = False
                Dim compteurCarteHaute As Boolean = False
                ReDim mainJoueur2(4)
                For i As Integer = 0 To 4
                    'Initialiser le random-number generator.
                    Randomize()
                    Dim value As Integer = 0
                    Dim trouve As Boolean
                    Do
                        trouve = True
                        ' Generer le random value between 0 and 51
                        value = Int(Int((51 * Rnd()) + 0))
                        For x As Integer = 0 To 4
                            If mainJoueur1(x) = value Then
                                trouve = False 'numero existant dans la combinaison1
                            End If
                            If mainJoueur2(x) = value Then
                                trouve = False 'numero existant dans la combinaison2
                            End If
                        Next
                    Loop While trouve = False
                    Select Case i
                        Case 0
                            If CheckBox1.Checked = False Then
                                mainJoueur2(i) = value
                                CarteBox1.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                            End If
                        Case 1
                            If CheckBox2.Checked = False Then
                                mainJoueur2(i) = value
                                CarteBox2.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                            End If
                        Case 2
                            If CheckBox3.Checked = False Then
                                mainJoueur2(i) = value
                                CarteBox3.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                            End If
                        Case 3
                            If CheckBox4.Checked = False Then
                                mainJoueur2(i) = value
                                CarteBox4.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                            End If
                        Case 4
                            If CheckBox5.Checked = False Then
                                mainJoueur2(i) = value
                                CarteBox5.Image = Image.FromFile(Application.StartupPath & "\image\" & paquetDeCartes(value))
                            End If
                    End Select
                Next

                'fusionner tour 1 + tour 2 dans main final
                For f As Integer = 0 To 4
                    Select Case f
                        Case 0
                            If CheckBox1.Checked = False Then
                                mainFinal(f) = mainJoueur2(f)
                            Else
                                mainFinal(f) = mainJoueur1(f)
                            End If
                        Case 1
                            If CheckBox2.Checked = False Then
                                mainFinal(f) = mainJoueur2(f)
                            Else
                                mainFinal(f) = mainJoueur1(f)
                            End If
                        Case 2
                            If CheckBox3.Checked = False Then
                                mainFinal(f) = mainJoueur2(f)
                            Else
                                mainFinal(f) = mainJoueur1(f)
                            End If
                        Case 3
                            If CheckBox4.Checked = False Then
                                mainFinal(f) = mainJoueur2(f)
                            Else
                                mainFinal(f) = mainJoueur1(f)
                            End If
                        Case 4
                            If CheckBox5.Checked = False Then
                                mainFinal(f) = mainJoueur2(f)
                            Else
                                mainFinal(f) = mainJoueur1(f)
                            End If
                    End Select
                Next
                'validation main final
                Dim positionPoint As Integer
                Dim chiffre(4) As Integer
                Dim lettre(4) As Char

                'IndexOf permet de trouve la position dans ce cas-ci le "."
                'Substring permet de lire une partie de la string .Substring(Debut, longueur)

                For g As Integer = 0 To 4
                    positionPoint = paquetDeCartes(mainFinal(g)).IndexOf(".")
                    If positionPoint = 2 Then
                        chiffre(g) = paquetDeCartes(mainFinal(g)).Substring(0, 1)
                        lettre(g) = paquetDeCartes(mainFinal(g)).Substring(1, 1)
                    Else
                        chiffre(g) = paquetDeCartes(mainFinal(g)).Substring(0, 2)
                        lettre(g) = paquetDeCartes(mainFinal(g)).Substring(2, 1)
                    End If

                Next

                '0=S 1=H 2=D 3=C
                For h As Integer = 0 To 4

                    compteurDeChiffre(chiffre(h) - 1) = compteurDeChiffre(chiffre(h) - 1) + 1
                    Select Case lettre(h)
                        Case "s"
                            compteurDeLettre(0) = compteurDeLettre(0) + 1
                        Case "h"
                            compteurDeLettre(1) = compteurDeLettre(1) + 1
                        Case "d"
                            compteurDeLettre(2) = compteurDeLettre(2) + 1
                        Case "c"
                            compteurDeLettre(3) = compteurDeLettre(3) + 1
                    End Select
                Next


                Array.Sort(compteurDeChiffre)
                For k As Integer = 12 To 7 Step -1
                    Select Case compteurDeChiffre(k)
                        Case 4  'QUATRE PAREILS
                            gainBox.Text = miseBox.Value * 25
                            creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                            MsgBox("Vous avez fait un gain! QUATRE PAREILS")
                            k = 7
                        Case 3
                            If compteurDeChiffre(k - 1) = 2 Then    'MAIN PLEINE
                                gainBox.Text = miseBox.Value * 20
                                creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                                MsgBox("Vous avez fait un gain! MAIN PLEINE")
                                k = 7
                            Else    'TROIS PAREILS
                                gainBox.Text = miseBox.Value * 5
                                creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                                MsgBox("Vous avez fait un gain! TROIS PAREILS")
                                k = 7
                            End If
                        Case 2
                            If compteurDeChiffre(k - 1) = 2 Then    'DEUX PAIRS
                                gainBox.Text = miseBox.Value * 3
                                creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                                MsgBox("Vous avez fait un gain! DEUX PAIRES")
                                k = 7
                            Else    'UNE PAIR
                                For f As Integer = 0 To 4 'Trouver si la pair est plus grande que VALET
                                    positionPoint = paquetDeCartes(mainFinal(f)).IndexOf(".")
                                    If positionPoint = 3 Then
                                        If Int(paquetDeCartes(mainFinal(f)).Substring(0, 2)) > 10 Then
                                            For y As Integer = f + 1 To 4
                                                positionPoint = paquetDeCartes(mainFinal(y)).IndexOf(".")
                                                If positionPoint = 3 And paquetDeCartes(mainFinal(f)).Substring(0, 2) = paquetDeCartes(mainFinal(y)).Substring(0, 2) Then
                                                    compteurCarteHaute = True
                                                    f = 5 'fin de sequence
                                                    y = 5 'fin de sequence
                                                End If
                                            Next
                                        End If
                                    Else
                                        If Int(paquetDeCartes(mainFinal(f)).Substring(0, 1)) = 1 Then
                                            For y As Integer = f + 1 To 4
                                                positionPoint = paquetDeCartes(mainFinal(y)).IndexOf(".")
                                                If positionPoint = 2 And paquetDeCartes(mainFinal(f)).Substring(0, 1) = paquetDeCartes(mainFinal(y)).Substring(0, 1) Then
                                                    compteurCarteHaute = True
                                                    f = 5 'fin de sequence
                                                    y = 5 'fin de sequence
                                                End If
                                            Next
                                        End If
                                    End If
                                Next
                                If compteurCarteHaute = True Then
                                    gainBox.Text = miseBox.Value * 1
                                    creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                                    MsgBox("Vous avez fait un gain! UNE PAIRE")
                                End If
                            End If
                    End Select
                Next

                'VAlider QUINTE
                Array.Sort(chiffre)
                Array.Sort(lettre)
                If chiffre(1) - chiffre(0) = 1 And chiffre(2) - chiffre(1) = 1 And chiffre(3) - chiffre(2) = 1 And chiffre(4) - chiffre(3) = 1 Then
                    If lettre(0) = lettre(1) And lettre(0) = lettre(2) And lettre(0) = lettre(3) And lettre(0) = lettre(4) Then
                        gainBox.Text = miseBox.Value * 50
                        creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                        MsgBox("Vous avez fait un gain! UNE QUINTE FLUSH")
                    Else
                        gainBox.Text = miseBox.Value * 10
                        creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                        MsgBox("Vous avez fait un gain! UNE QUINTE")
                    End If
                End If

                If chiffre(0) = 1 And chiffre(2) - chiffre(1) = 1 And chiffre(3) - chiffre(2) = 1 And chiffre(4) - chiffre(3) = 1 And chiffre(4) = 13 Then
                    If lettre(0) = lettre(1) And lettre(0) = lettre(2) And lettre(0) = lettre(3) And lettre(0) = lettre(4) Then
                        gainBox.Text = miseBox.Value * 100
                        creditBox.Text = Int(creditBox.Text) + Int(gainBox.Text)
                        MsgBox("Vous avez fait un gain! UNE QUINTE FLUSH ROYALE")
                    End If
                End If

            End If

            reponse = MsgBox("Voulez-vous jouer un autre tour?", vbYesNo) 'reset du jeu
            If reponse = 6 Then
                CarteBox1.Image = Image.FromFile(Application.StartupPath & "\image\doscarte.jpg")
                CarteBox2.Image = Image.FromFile(Application.StartupPath & "\image\doscarte.jpg")
                CarteBox3.Image = Image.FromFile(Application.StartupPath & "\image\doscarte.jpg")
                CarteBox4.Image = Image.FromFile(Application.StartupPath & "\image\doscarte.jpg")
                CarteBox5.Image = Image.FromFile(Application.StartupPath & "\image\doscarte.jpg")
                CheckBox1.Checked = False
                CheckBox2.Checked = False
                CheckBox3.Checked = False
                CheckBox4.Checked = False
                CheckBox5.Checked = False
                CheckBox1.Enabled = False
                CheckBox2.Enabled = False
                CheckBox3.Enabled = False
                CheckBox4.Enabled = False
                CheckBox5.Enabled = False
                debutJeu = False
                deuxiemeMain = True
                gainBox.Text = ""
                ReDim compteurDeChiffre(12)
                ReDim compteurDeLettre(3)
            End If

        Else
            MsgBox("Veuillez cliquez sur le bouton JOUER pour débuter")
        End If
    End Sub

    Private Sub BtnQuitter_Click(sender As Object, e As EventArgs) Handles BtnQuitter.Click ' boutton quitter  msgBox qui valid le fait de terminer le prog

        Dim reponse As Integer
        reponse = MsgBox("Êtes-vous certain de vouloir quitter?", vbYesNo)

        If reponse = 6 Then
            Application.Exit()
        End If
    End Sub
End Class

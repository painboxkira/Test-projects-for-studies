program BourseDesValeurs;

const
  MaxValeurs = 100;

type
  Enregistrement = record
    Date: string[10];
    Nom: string[30];
    ValeurInitiale: real;
    ValeurFinale: real;
    Pourcentage: real;
  end;

  TableauValeurs = array[1..MaxValeurs] of Enregistrement;

var
  Valeurs: TableauValeurs;
  NbValeurs: integer;
  choix: integer;

procedure SaisieDonnees(var Valeurs: TableauValeurs; var NbValeurs: integer);
var
  i: integer;
begin
  writeln('Entrez le nombre de valeurs à enregistrer: ');
  readln(NbValeurs);
  for i := 1 to NbValeurs do
  begin
    writeln('Valeur ', i, ':');
    write('Date (JJ/MM/AAAA): '); readln(Valeurs[i].Date);
    write('Nom: '); readln(Valeurs[i].Nom);
    write('Valeur Initiale: '); readln(Valeurs[i].ValeurInitiale);
    write('Valeur Finale: '); readln(Valeurs[i].ValeurFinale);
    Valeurs[i].Pourcentage := 0; // Initialisation
  end;
end;

procedure CalculPourcentages(var Valeurs: TableauValeurs; NbValeurs: integer);
var
  i: integer;
begin
  for i := 1 to NbValeurs do
  begin
    if Valeurs[i].ValeurInitiale <> 0 then
      Valeurs[i].Pourcentage := ((Valeurs[i].ValeurFinale - Valeurs[i].ValeurInitiale) / Valeurs[i].ValeurInitiale) * 100
    else
      Valeurs[i].Pourcentage := 0; // Избегаем деления на ноль
  end;
end;

procedure AfficherValeurs(Valeurs: TableauValeurs; NbValeurs: integer);
var
  i: integer;
begin
  writeln('---------------------------------------------------------');
  writeln('Date       | Nom       | Initiale | Finale  | % Changement');
  writeln('---------------------------------------------------------');
  for i := 1 to NbValeurs do
  begin
    with Valeurs[i] do
    begin
      writeln(Date:10, ' | ', Nom:10, ' | ', ValeurInitiale:8:2, ' | ', ValeurFinale:8:2, ' | ', Pourcentage:8:2);
    end;
  end;
end;

procedure Menu();
begin
  repeat
    writeln('MENU PRINCIPAL');
    writeln('1. Saisie des données');
    writeln('2. Calcul des pourcentages');
    writeln('3. Affichage des résultats');
    writeln('4. Quitter');
    writeln('Entrez votre choix: ');
    readln(choix);

    case choix of
      1: SaisieDonnees(Valeurs, NbValeurs);
      2: CalculPourcentages(Valeurs, NbValeurs);
      3: AfficherValeurs(Valeurs, NbValeurs);
    end;
  until choix = 4;
end;

begin
  NbValeurs := 0;
  Menu();
end.

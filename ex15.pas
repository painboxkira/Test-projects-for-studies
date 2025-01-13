program GestionProduitsPetroliers;

const
  NbProduits = 50; // Number of products

type
  Tableau = array[1..NbProduits] of integer;

var
  V1, V2, V3, TotalProduits: Tableau;
  TotalStations: array[1..3] of integer;
  MoyenneProduits: Tableau;
  i, choix: integer;

procedure SaisieDonnees(var V1, V2, V3: Tableau);
begin
  for i := 1 to NbProduits do
  begin
    writeln('Produit ', i, ' - Station 1: ');
    readln(V1[i]);
    writeln('Produit ', i, ' - Station 2: ');
    readln(V2[i]);
    writeln('Produit ', i, ' - Station 3: ');
    readln(V3[i]);
  end;
end;

procedure CalculTotalProduits(var TotalProduits: Tableau; V1, V2, V3: Tableau);
begin
  for i := 1 to NbProduits do
    TotalProduits[i] := V1[i] + V2[i] + V3[i];
end;

procedure CalculTotalStations(var TotalStations: array of integer; V1, V2, V3: Tableau);
begin
  TotalStations[1] := 0;
  TotalStations[2] := 0;
  TotalStations[3] := 0;
  for i := 1 to NbProduits do
  begin
    TotalStations[1] := TotalStations[1] + V1[i];
    TotalStations[2] := TotalStations[2] + V2[i];
    TotalStations[3] := TotalStations[3] + V3[i];
  end;
end;

procedure CalculMoyenneProduits(var MoyenneProduits: Tableau; TotalProduits: Tableau);
begin
  for i := 1 to NbProduits do
    MoyenneProduits[i] := TotalProduits[i] div 3;
end;

procedure EditionResultats(TotalProduits, MoyenneProduits: Tableau);
begin
  writeln('Num Produit | Total Vendu | Moyenne Vendu');
  for i := 1 to NbProduits do
    writeln(i:10, TotalProduits[i]:13, MoyenneProduits[i]:15);
end;

procedure Menu();
begin
  repeat
    writeln('MENU PRINCIPAL');
    writeln('1. Saisie des données');
    writeln('2. Calcul Total par Produit');
    writeln('3. Calcul Total par Station');
    writeln('4. Calcul Moyenne par Produit');
    writeln('5. Edition des Résultats');
    writeln('6. Quitter');
    writeln('Entrez votre choix: ');
    readln(choix);

    case choix of
      1: SaisieDonnees(V1, V2, V3);
      2: CalculTotalProduits(TotalProduits, V1, V2, V3);
      3: CalculTotalStations(TotalStations, V1, V2, V3);
      4: CalculMoyenneProduits(MoyenneProduits, TotalProduits);
      5: EditionResultats(TotalProduits, MoyenneProduits);
    end;
  until choix = 6;
end;

begin
  Menu();
end.

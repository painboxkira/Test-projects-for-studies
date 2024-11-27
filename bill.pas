program GarageFacture;

var
    choix, nombre_crevaisons, nombre_equilibrages, nombre_pneus: Integer;
    prix_pont_avant, prix_pneu, sous_total, frais_service, taxe, total, montant_paye, monnaie: Real;

begin
    sous_total := 0;

    // Service 1 : Parallélisme des roues
    Write('Parallélisme des roues (1 pour inclus, 0 pour exclu) : ');
    ReadLn(choix);
    if choix = 1 then
        sous_total := sous_total + 60;

    // Service 2 : Nettoyage et entretien radiateur
    Write('Nettoyage et entretien radiateur (1 pour inclus, 0 pour exclu) : ');
    ReadLn(choix);
    if choix = 1 then
        sous_total := sous_total + 80;

    // Service 3 : Réglage des phares
    Write('Réglage des phares (1 pour inclus, 0 pour exclu) : ');
    ReadLn(choix);
    if choix = 1 then
        sous_total := sous_total + 20;

    // Service 4 : Réparation de crevaison
    Write('Nombre de crevaisons réparées : ');
    ReadLn(nombre_crevaisons);
    sous_total := sous_total + (nombre_crevaisons * 15);

    // Service 5 : Équilibrage d’une roue
    Write('Nombre d’équilibrages effectués : ');
    ReadLn(nombre_equilibrages);
    sous_total := sous_total + (nombre_equilibrages * 10);

    // Service 6 : Réparation du pont avant
    Write('Prix de la réparation du pont avant (0 si non inclus) : ');
    ReadLn(prix_pont_avant);
    sous_total := sous_total + prix_pont_avant;

    // Service 7 : Achat de pneus
    Write('Nombre de pneus achetés : ');
    ReadLn(nombre_pneus);
    Write('Prix unitaire d’un pneu : ');
    ReadLn(prix_pneu);
    sous_total := sous_total + (nombre_pneus * prix_pneu);

    // Calculs
    frais_service := sous_total * 0.05;
    taxe := sous_total * 0.10;
    total := sous_total + frais_service + taxe;

    // Paiement
    Write('Montant payé par le client : ');
    ReadLn(montant_paye);
    monnaie := montant_paye - total;

    // Affichage de la facture
    WriteLn('------------------------------');
    WriteLn('Facture du garage');
    WriteLn('Sous-total : ', sous_total:0:2, ' DH');
    WriteLn('Frais de service (5%) : ', frais_service:0:2, ' DH');
    WriteLn('Taxe (10%) : ', taxe:0:2, ' DH');
    WriteLn('Total à payer : ', total:0:2, ' DH');
    WriteLn('Montant payé : ', montant_paye:0:2, ' DH');
    WriteLn('Monnaie à rendre : ', monnaie:0:2, ' DH');
    WriteLn('------------------------------');
end.
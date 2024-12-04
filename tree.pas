program ForestManagement;

type
  Tree = record
    Name: string[20];
    Count: Integer;
    GrowthRate: Real; // Wood growth per tree (m^3/year)
  end;

var
  Trees: array[1..100] of Tree; // Array to store tree types
  TotalWood: Real;
  Years, i, j, NumTrees: Integer;
  MinTreeCount: Integer;

begin
  // Step 1: Input forest data
  WriteLn('Enter the number of tree types:');
  ReadLn(NumTrees);

  for i := 1 to NumTrees do
  begin
    WriteLn('Enter details for tree type ', i, ':');
    Write('Name: ');
    ReadLn(Trees[i].Name);
    Write('Initial Count: ');
    ReadLn(Trees[i].Count);
    Write('Annual Growth Rate (m^3/tree): ');
    ReadLn(Trees[i].GrowthRate);
  end;

  Write('Enter the minimum tree count for sustainability: ');
  ReadLn(MinTreeCount);

  Write('Enter the number of years to simulate: ');
  ReadLn(Years);

  // Step 2: Simulate growth over the years
  for j := 1 to Years do
  begin
    WriteLn('Year ', j, ':');
    TotalWood := 0;

    for i := 1 to NumTrees do
    begin
      // Calculate total wood produced by this tree type
      TotalWood := TotalWood + (Trees[i].Count * Trees[i].GrowthRate);

      // Output details for this tree type
      WriteLn('  ', Trees[i].Name, ': ', Trees[i].Count, ' trees, ',
              Trees[i].Count * Trees[i].GrowthRate:0:2, ' m^3 of wood');

      // Check if tree count is below minimum
      if Trees[i].Count < MinTreeCount then
      begin
        WriteLn('  ', Trees[i].Name, ' count below minimum. Replenishing...');
        Trees[i].Count := MinTreeCount; // Replenish to minimum count
      end;

      // Assume 5% of trees die off each year
      Trees[i].Count := Round(Trees[i].Count * 0.95);
    end;

    // Output total wood produced this year
    WriteLn('Total wood produced this year: ', TotalWood:0:2, ' m^3');
    WriteLn;
  end;

  WriteLn('Simulation complete.');
end.
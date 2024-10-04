unit TestMonolito;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMonolitoFinanceiroTest = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

procedure TMonolitoFinanceiroTest.Setup;
begin
end;

procedure TMonolitoFinanceiroTest.TearDown;
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TMonolitoFinanceiroTest);

end.

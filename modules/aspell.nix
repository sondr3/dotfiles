self: super:
{
  aspellWithDicts = with super; aspellWithDicts(ps: with ps; [ en nb ]);
}

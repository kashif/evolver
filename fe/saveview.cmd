// saveview.cmd

// Evolver command for saving current view matrix in proper form
//   for reading in.
// Typical usage: saveview >>> "viewfile.cmd"
// Then to restore view: read "viewfile.cmd"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

saveview := {
  printf "oldautodisplay := (autodisplay);\n";
  printf "autodisplay off\n";
  printf "view_matrix[1][1] := %f\n",view_matrix[1][1];
  printf "view_matrix[1][2] := %f\n",view_matrix[1][2];
  printf "view_matrix[1][3] := %f\n",view_matrix[1][3];
  printf "view_matrix[1][4] := %f\n",view_matrix[1][4];
  printf "view_matrix[2][1] := %f\n",view_matrix[2][1];
  printf "view_matrix[2][2] := %f\n",view_matrix[2][2];
  printf "view_matrix[2][3] := %f\n",view_matrix[2][3];
  printf "view_matrix[2][4] := %f\n",view_matrix[2][4];
  printf "view_matrix[3][1] := %f\n",view_matrix[3][1];
  printf "view_matrix[3][2] := %f\n",view_matrix[3][2];
  printf "view_matrix[3][3] := %f\n",view_matrix[3][3];
  printf "view_matrix[3][4] := %f\n",view_matrix[3][4];
  printf "view_matrix[4][1] := %f\n",view_matrix[4][1];
  printf "view_matrix[4][2] := %f\n",view_matrix[4][2];
  printf "view_matrix[4][3] := %f\n",view_matrix[4][3];
  printf "view_matrix[4][4] := %f\n",view_matrix[4][4];
  printf "if oldautodisplay then autodisplay on\n";
}

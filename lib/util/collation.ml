(* Weights of the default table of the Unicode collation algorithm (DUCET,
   allkeys.txt 10.0.0), primary then secondary, of the lowercase letters
   without canonical decomposition and of the combining marks of
   U+0000..U+052F and U+1E00..U+1EFF; the primary weights are multiplied by
   16, so that a tailoring can insert letters between two of them *)
let weights =
  let t = Hashtbl.create 512 in
  List.iter
    (fun (c, p, s) -> Hashtbl.replace t c (List.map (fun w -> w lsl 4) p, s))
    [
      (0x0061, [ 0x1CAD ], [ 0x20 ]);
      (0x0062, [ 0x1CC6 ], [ 0x20 ]);
      (0x0063, [ 0x1CE0 ], [ 0x20 ]);
      (0x0064, [ 0x1CF5 ], [ 0x20 ]);
      (0x0065, [ 0x1D10 ], [ 0x20 ]);
      (0x0066, [ 0x1D4B ], [ 0x20 ]);
      (0x0067, [ 0x1D5A ], [ 0x20 ]);
      (0x0068, [ 0x1D7E ], [ 0x20 ]);
      (0x0069, [ 0x1D98 ], [ 0x20 ]);
      (0x006A, [ 0x1DB2 ], [ 0x20 ]);
      (0x006B, [ 0x1DCB ], [ 0x20 ]);
      (0x006C, [ 0x1DDD ], [ 0x20 ]);
      (0x006D, [ 0x1E10 ], [ 0x20 ]);
      (0x006E, [ 0x1E1F ], [ 0x20 ]);
      (0x006F, [ 0x1E43 ], [ 0x20 ]);
      (0x0070, [ 0x1E72 ], [ 0x20 ]);
      (0x0071, [ 0x1E87 ], [ 0x20 ]);
      (0x0072, [ 0x1E99 ], [ 0x20 ]);
      (0x0073, [ 0x1ED7 ], [ 0x20 ]);
      (0x0074, [ 0x1EFB ], [ 0x20 ]);
      (0x0075, [ 0x1F1B ], [ 0x20 ]);
      (0x0076, [ 0x1F49 ], [ 0x20 ]);
      (0x0077, [ 0x1F5B ], [ 0x20 ]);
      (0x0078, [ 0x1F65 ], [ 0x20 ]);
      (0x0079, [ 0x1F71 ], [ 0x20 ]);
      (0x007A, [ 0x1F87 ], [ 0x20 ]);
      (0x00AA, [ 0x1CAD ], [ 0x20 ]);
      (0x00B5, [ 0x2031 ], [ 0x20 ]);
      (0x00BA, [ 0x1E43 ], [ 0x20 ]);
      (0x00DF, [ 0x1ED7; 0x1ED7 ], [ 0x20; 0x111; 0x20 ]);
      (0x00E6, [ 0x1CAD; 0x1D10 ], [ 0x20; 0x111; 0x20 ]);
      (0x00F0, [ 0x1CF5 ], [ 0x20; 0x111 ]);
      (0x00F8, [ 0x1E43 ], [ 0x20; 0x2F ]);
      (0x00FE, [ 0x1FB6 ], [ 0x20 ]);
      (0x0111, [ 0x1CF5 ], [ 0x20; 0x39 ]);
      (0x0127, [ 0x1D7E ], [ 0x20; 0x39 ]);
      (0x0131, [ 0x1D9C ], [ 0x20 ]);
      (0x0133, [ 0x1D98; 0x1DB2 ], [ 0x20; 0x20 ]);
      (0x0138, [ 0x1E95 ], [ 0x20 ]);
      (0x0140, [ 0x1DDD ], [ 0x20; 0x111 ]);
      (0x0142, [ 0x1DDD ], [ 0x20; 0x39 ]);
      (0x0149, [ 0x1FE4; 0x1E1F ], [ 0x20; 0x20 ]);
      (0x014B, [ 0x1E3E ], [ 0x20 ]);
      (0x0153, [ 0x1E43; 0x1D10 ], [ 0x20; 0x111; 0x20 ]);
      (0x0167, [ 0x1F00 ], [ 0x20 ]);
      (0x017F, [ 0x1ED7 ], [ 0x20; 0x112 ]);
      (0x0180, [ 0x1CCE ], [ 0x20 ]);
      (0x0183, [ 0x1CDB ], [ 0x20 ]);
      (0x0185, [ 0x1FD7 ], [ 0x20 ]);
      (0x0188, [ 0x1CEB ], [ 0x20 ]);
      (0x018C, [ 0x1D06 ], [ 0x20 ]);
      (0x018D, [ 0x1F87; 0x1F5B ], [ 0x20; 0x20 ]);
      (0x0192, [ 0x1D54 ], [ 0x20 ]);
      (0x0195, [ 0x1D86 ], [ 0x20 ]);
      (0x0199, [ 0x1DD1 ], [ 0x20 ]);
      (0x019A, [ 0x1DE8 ], [ 0x20 ]);
      (0x019B, [ 0x1E08 ], [ 0x20 ]);
      (0x019E, [ 0x1E2E ], [ 0x20 ]);
      (0x01A3, [ 0x1D7A ], [ 0x20 ]);
      (0x01A5, [ 0x1E7B ], [ 0x20 ]);
      (0x01A8, [ 0x1FCF ], [ 0x20 ]);
      (0x01AA, [ 0x1EEE ], [ 0x20 ]);
      (0x01AB, [ 0x1F06 ], [ 0x20 ]);
      (0x01AD, [ 0x1F0A ], [ 0x20 ]);
      (0x01B4, [ 0x1F7D ], [ 0x20 ]);
      (0x01B6, [ 0x1F8C ], [ 0x20 ]);
      (0x01B9, [ 0x1FA9 ], [ 0x20 ]);
      (0x01BA, [ 0x1FAE ], [ 0x20 ]);
      (0x01BB, [ 0x1FC8 ], [ 0x20 ]);
      (0x01BD, [ 0x1FD3 ], [ 0x20 ]);
      (0x01BE, [ 0x1EFB; 0x1ED7 ], [ 0x20; 0x20 ]);
      (0x01BF, [ 0x1FBC ], [ 0x20 ]);
      (0x01C0, [ 0x1FFF ], [ 0x20 ]);
      (0x01C1, [ 0x2003 ], [ 0x20 ]);
      (0x01C2, [ 0x2007 ], [ 0x20 ]);
      (0x01C3, [ 0x200B ], [ 0x20 ]);
      (0x01C6, [ 0x1CF5; 0x1F87 ], [ 0x20; 0x20; 0x28 ]);
      (0x01C9, [ 0x1DDD; 0x1DB2 ], [ 0x20; 0x20 ]);
      (0x01CC, [ 0x1E1F; 0x1DB2 ], [ 0x20; 0x20 ]);
      (0x01DD, [ 0x1D1E ], [ 0x20 ]);
      (0x01E5, [ 0x1D67 ], [ 0x20 ]);
      (0x01F3, [ 0x1CF5; 0x1F87 ], [ 0x20; 0x20 ]);
      (0x021D, [ 0x1F83 ], [ 0x20 ]);
      (0x0221, [ 0x1D0A ], [ 0x20 ]);
      (0x0223, [ 0x1E6D ], [ 0x20 ]);
      (0x0225, [ 0x1F92 ], [ 0x20 ]);
      (0x0234, [ 0x1DFE ], [ 0x20 ]);
      (0x0235, [ 0x1E38 ], [ 0x20 ]);
      (0x0236, [ 0x1F12 ], [ 0x20 ]);
      (0x0237, [ 0x1DB6 ], [ 0x20 ]);
      (0x0238, [ 0x1CF5; 0x1CC6 ], [ 0x20; 0x20 ]);
      (0x0239, [ 0x1E87; 0x1E72 ], [ 0x20; 0x20 ]);
      (0x023C, [ 0x1CE5 ], [ 0x20 ]);
      (0x023F, [ 0x1EE2 ], [ 0x20 ]);
      (0x0240, [ 0x1F9E ], [ 0x20 ]);
      (0x0242, [ 0x1FDF ], [ 0x20 ]);
      (0x0247, [ 0x1D17 ], [ 0x20 ]);
      (0x0249, [ 0x1DBB ], [ 0x20 ]);
      (0x024B, [ 0x1E91 ], [ 0x20 ]);
      (0x024D, [ 0x1EA5 ], [ 0x20 ]);
      (0x024F, [ 0x1F79 ], [ 0x20 ]);
      (0x0250, [ 0x1CB7 ], [ 0x20 ]);
      (0x0251, [ 0x1CBB ], [ 0x20 ]);
      (0x0252, [ 0x1CC1 ], [ 0x20 ]);
      (0x0253, [ 0x1CD7 ], [ 0x20 ]);
      (0x0254, [ 0x1E56 ], [ 0x20 ]);
      (0x0255, [ 0x1CEF ], [ 0x20 ]);
      (0x0256, [ 0x1CFD ], [ 0x20 ]);
      (0x0257, [ 0x1D01 ], [ 0x20 ]);
      (0x0258, [ 0x1D2D ], [ 0x20 ]);
      (0x0259, [ 0x1D23 ], [ 0x20 ]);
      (0x025A, [ 0x1D31 ], [ 0x20 ]);
      (0x025B, [ 0x1D28 ], [ 0x20 ]);
      (0x025C, [ 0x1D35 ], [ 0x20 ]);
      (0x025D, [ 0x1D3B ], [ 0x20 ]);
      (0x025E, [ 0x1D3F ], [ 0x20 ]);
      (0x025F, [ 0x1DC3 ], [ 0x20 ]);
      (0x0260, [ 0x1D6C ], [ 0x20 ]);
      (0x0261, [ 0x1D5E ], [ 0x20 ]);
      (0x0262, [ 0x1D63 ], [ 0x20 ]);
      (0x0263, [ 0x1D76 ], [ 0x20 ]);
      (0x0264, [ 0x1D47 ], [ 0x20 ]);
      (0x0265, [ 0x1F2E ], [ 0x20 ]);
      (0x0266, [ 0x1D8B ], [ 0x20 ]);
      (0x0267, [ 0x1D92 ], [ 0x20 ]);
      (0x0268, [ 0x1DA7 ], [ 0x20 ]);
      (0x0269, [ 0x1DAD ], [ 0x20 ]);
      (0x026A, [ 0x1DA0 ], [ 0x20 ]);
      (0x026B, [ 0x1DED ], [ 0x20 ]);
      (0x026C, [ 0x1DF3 ], [ 0x20 ]);
      (0x026D, [ 0x1DF9 ], [ 0x20 ]);
      (0x026E, [ 0x1E03 ], [ 0x20 ]);
      (0x026F, [ 0x1F3A ], [ 0x20 ]);
      (0x0270, [ 0x1F40 ], [ 0x20 ]);
      (0x0271, [ 0x1E17 ], [ 0x20 ]);
      (0x0272, [ 0x1E2A ], [ 0x20 ]);
      (0x0273, [ 0x1E34 ], [ 0x20 ]);
      (0x0274, [ 0x1E23 ], [ 0x20 ]);
      (0x0275, [ 0x1E63 ], [ 0x20 ]);
      (0x0276, [ 0x1E4A ], [ 0x20 ]);
      (0x0277, [ 0x1E68 ], [ 0x20 ]);
      (0x0278, [ 0x1E82 ], [ 0x20 ]);
      (0x0279, [ 0x1EAA ], [ 0x20 ]);
      (0x027A, [ 0x1EAF ], [ 0x20 ]);
      (0x027B, [ 0x1EB4 ], [ 0x20 ]);
      (0x027C, [ 0x1EB9 ], [ 0x20 ]);
      (0x027D, [ 0x1EBD ], [ 0x20 ]);
      (0x027E, [ 0x1EC2 ], [ 0x20 ]);
      (0x027F, [ 0x1EC7 ], [ 0x20 ]);
      (0x0280, [ 0x1E9E ], [ 0x20 ]);
      (0x0281, [ 0x1ED0 ], [ 0x20 ]);
      (0x0282, [ 0x1EDE ], [ 0x20 ]);
      (0x0283, [ 0x1EE8 ], [ 0x20 ]);
      (0x0284, [ 0x1DC7 ], [ 0x20 ]);
      (0x0285, [ 0x1EF2 ], [ 0x20 ]);
      (0x0286, [ 0x1EF7 ], [ 0x20 ]);
      (0x0287, [ 0x1F17 ], [ 0x20 ]);
      (0x0288, [ 0x1F0E ], [ 0x20 ]);
      (0x0289, [ 0x1F26 ], [ 0x20 ]);
      (0x028A, [ 0x1F44 ], [ 0x20 ]);
      (0x028B, [ 0x1F50 ], [ 0x20 ]);
      (0x028C, [ 0x1F57 ], [ 0x20 ]);
      (0x028D, [ 0x1F61 ], [ 0x20 ]);
      (0x028E, [ 0x1E0C ], [ 0x20 ]);
      (0x028F, [ 0x1F75 ], [ 0x20 ]);
      (0x0290, [ 0x1F96 ], [ 0x20 ]);
      (0x0291, [ 0x1F9A ], [ 0x20 ]);
      (0x0292, [ 0x1FA4 ], [ 0x20 ]);
      (0x0293, [ 0x1FB2 ], [ 0x20 ]);
      (0x0294, [ 0x1FDB ], [ 0x20 ]);
      (0x0295, [ 0x1FEA ], [ 0x20 ]);
      (0x0296, [ 0x1FFB ], [ 0x20 ]);
      (0x0297, [ 0x200F ], [ 0x20 ]);
      (0x0298, [ 0x2013 ], [ 0x20 ]);
      (0x0299, [ 0x1CCA ], [ 0x20 ]);
      (0x029A, [ 0x1D43 ], [ 0x20 ]);
      (0x029B, [ 0x1D70 ], [ 0x20 ]);
      (0x029C, [ 0x1D82 ], [ 0x20 ]);
      (0x029D, [ 0x1DBF ], [ 0x20 ]);
      (0x029E, [ 0x1DD9 ], [ 0x20 ]);
      (0x029F, [ 0x1DE1 ], [ 0x20 ]);
      (0x02A0, [ 0x1E8D ], [ 0x20 ]);
      (0x02A1, [ 0x1FF3 ], [ 0x20 ]);
      (0x02A2, [ 0x1FF7 ], [ 0x20 ]);
      (0x02A3, [ 0x1CF5; 0x1F87 ], [ 0x20; 0x20 ]);
      (0x02A4, [ 0x1CF5; 0x1FA4 ], [ 0x20; 0x20 ]);
      (0x02A5, [ 0x1CF5; 0x1F9A ], [ 0x20; 0x20 ]);
      (0x02A6, [ 0x1EFB; 0x1ED7 ], [ 0x20; 0x20 ]);
      (0x02A7, [ 0x1EFB; 0x1EE8 ], [ 0x20; 0x20 ]);
      (0x02A8, [ 0x1EFB; 0x1CEF ], [ 0x20; 0x20 ]);
      (0x02A9, [ 0x1D4B; 0x1E3E ], [ 0x20; 0x20 ]);
      (0x02AA, [ 0x1DDD; 0x1ED7 ], [ 0x20; 0x20 ]);
      (0x02AB, [ 0x1DDD; 0x1F87 ], [ 0x20; 0x20 ]);
      (0x02AC, [ 0x2017 ], [ 0x20 ]);
      (0x02AD, [ 0x201B ], [ 0x20 ]);
      (0x02AE, [ 0x1F32 ], [ 0x20 ]);
      (0x02AF, [ 0x1F36 ], [ 0x20 ]);
      (0x0300, [], [ 0x25 ]);
      (0x0301, [], [ 0x24 ]);
      (0x0302, [], [ 0x27 ]);
      (0x0303, [], [ 0x2D ]);
      (0x0304, [], [ 0x32 ]);
      (0x0305, [], [ 0x3A ]);
      (0x0306, [], [ 0x26 ]);
      (0x0307, [], [ 0x2E ]);
      (0x0308, [], [ 0x2B ]);
      (0x0309, [], [ 0x3B ]);
      (0x030A, [], [ 0x29 ]);
      (0x030B, [], [ 0x2C ]);
      (0x030C, [], [ 0x28 ]);
      (0x030D, [], [ 0x33 ]);
      (0x030E, [], [ 0x33 ]);
      (0x030F, [], [ 0x3C ]);
      (0x0310, [], [ 0x3D ]);
      (0x0311, [], [ 0x3E ]);
      (0x0312, [], [ 0x33 ]);
      (0x0313, [], [ 0x22 ]);
      (0x0314, [], [ 0x23 ]);
      (0x0315, [], [ 0x33 ]);
      (0x0316, [], [ 0x34 ]);
      (0x0317, [], [ 0x34 ]);
      (0x0318, [], [ 0x34 ]);
      (0x0319, [], [ 0x34 ]);
      (0x031A, [], [ 0x33 ]);
      (0x031B, [], [ 0x3F ]);
      (0x031C, [], [ 0x34 ]);
      (0x031D, [], [ 0x34 ]);
      (0x031E, [], [ 0x34 ]);
      (0x031F, [], [ 0x34 ]);
      (0x0320, [], [ 0x34 ]);
      (0x0321, [], [ 0x40 ]);
      (0x0322, [], [ 0x41 ]);
      (0x0323, [], [ 0x42 ]);
      (0x0324, [], [ 0x43 ]);
      (0x0325, [], [ 0x44 ]);
      (0x0326, [], [ 0x45 ]);
      (0x0327, [], [ 0x30 ]);
      (0x0328, [], [ 0x31 ]);
      (0x0329, [], [ 0x34 ]);
      (0x032A, [], [ 0x34 ]);
      (0x032B, [], [ 0x34 ]);
      (0x032C, [], [ 0x34 ]);
      (0x032D, [], [ 0x46 ]);
      (0x032E, [], [ 0x47 ]);
      (0x032F, [], [ 0x34 ]);
      (0x0330, [], [ 0x48 ]);
      (0x0331, [], [ 0x49 ]);
      (0x0332, [], [ 0x21 ]);
      (0x0333, [], [ 0x34 ]);
      (0x0334, [], [ 0x4A ]);
      (0x0335, [], [ 0x39 ]);
      (0x0336, [], [ 0x35 ]);
      (0x0337, [], [ 0x35 ]);
      (0x0338, [], [ 0x2F ]);
      (0x0339, [], [ 0x4B ]);
      (0x033A, [], [ 0x34 ]);
      (0x033B, [], [ 0x34 ]);
      (0x033C, [], [ 0x34 ]);
      (0x033D, [], [ 0x33 ]);
      (0x033E, [], [ 0x33 ]);
      (0x033F, [], [ 0x33 ]);
      (0x0342, [], [ 0x2A ]);
      (0x0345, [], [ 0x4C ]);
      (0x0346, [], [ 0x33 ]);
      (0x0347, [], [ 0x34 ]);
      (0x0348, [], [ 0x34 ]);
      (0x0349, [], [ 0x34 ]);
      (0x034A, [], [ 0x33 ]);
      (0x034B, [], [ 0x33 ]);
      (0x034C, [], [ 0x33 ]);
      (0x034D, [], [ 0x34 ]);
      (0x034E, [], [ 0x34 ]);
      (0x0350, [], [ 0x33 ]);
      (0x0351, [], [ 0x33 ]);
      (0x0352, [], [ 0x33 ]);
      (0x0353, [], [ 0x34 ]);
      (0x0354, [], [ 0x34 ]);
      (0x0355, [], [ 0x34 ]);
      (0x0356, [], [ 0x34 ]);
      (0x0357, [], [ 0x33 ]);
      (0x0358, [], [ 0x4D ]);
      (0x0359, [], [ 0x34 ]);
      (0x035A, [], [ 0x34 ]);
      (0x035B, [], [ 0x33 ]);
      (0x035C, [], [ 0x34 ]);
      (0x035D, [], [ 0x33 ]);
      (0x035E, [], [ 0x33 ]);
      (0x035F, [], [ 0x34 ]);
      (0x0360, [], [ 0x4E ]);
      (0x0361, [], [ 0x4F ]);
      (0x0362, [], [ 0x34 ]);
      (0x0363, [ 0x1CAD ], [ 0x20 ]);
      (0x0364, [ 0x1D10 ], [ 0x20 ]);
      (0x0365, [ 0x1D98 ], [ 0x20 ]);
      (0x0366, [ 0x1E43 ], [ 0x20 ]);
      (0x0367, [ 0x1F1B ], [ 0x20 ]);
      (0x0368, [ 0x1CE0 ], [ 0x20 ]);
      (0x0369, [ 0x1CF5 ], [ 0x20 ]);
      (0x036A, [ 0x1D7E ], [ 0x20 ]);
      (0x036B, [ 0x1E10 ], [ 0x20 ]);
      (0x036C, [ 0x1E99 ], [ 0x20 ]);
      (0x036D, [ 0x1EFB ], [ 0x20 ]);
      (0x036E, [ 0x1F49 ], [ 0x20 ]);
      (0x036F, [ 0x1F65 ], [ 0x20 ]);
      (0x0371, [ 0x2029 ], [ 0x20 ]);
      (0x0373, [ 0x204A ], [ 0x20 ]);
      (0x0377, [ 0x2026 ], [ 0x20 ]);
      (0x037A, [ 0x202C ], [ 0x20 ]);
      (0x037B, [ 0x203F ], [ 0x20 ]);
      (0x037C, [ 0x203E ], [ 0x20 ]);
      (0x037D, [ 0x2040 ], [ 0x20 ]);
      (0x03B1, [ 0x201F ], [ 0x20 ]);
      (0x03B2, [ 0x2020 ], [ 0x20 ]);
      (0x03B3, [ 0x2021 ], [ 0x20 ]);
      (0x03B4, [ 0x2023 ], [ 0x20 ]);
      (0x03B5, [ 0x2024 ], [ 0x20 ]);
      (0x03B6, [ 0x2028 ], [ 0x20 ]);
      (0x03B7, [ 0x202A ], [ 0x20 ]);
      (0x03B8, [ 0x202B ], [ 0x20 ]);
      (0x03B9, [ 0x202C ], [ 0x20 ]);
      (0x03BA, [ 0x202E ], [ 0x20 ]);
      (0x03BB, [ 0x202F ], [ 0x20 ]);
      (0x03BC, [ 0x2031 ], [ 0x20 ]);
      (0x03BD, [ 0x2032 ], [ 0x20 ]);
      (0x03BE, [ 0x2033 ], [ 0x20 ]);
      (0x03BF, [ 0x2034 ], [ 0x20 ]);
      (0x03C0, [ 0x2035 ], [ 0x20 ]);
      (0x03C1, [ 0x203A ], [ 0x20 ]);
      (0x03C2, [ 0x203D ], [ 0x20 ]);
      (0x03C3, [ 0x203D ], [ 0x20 ]);
      (0x03C4, [ 0x2041 ], [ 0x20 ]);
      (0x03C5, [ 0x2042 ], [ 0x20 ]);
      (0x03C6, [ 0x2043 ], [ 0x20 ]);
      (0x03C7, [ 0x2044 ], [ 0x20 ]);
      (0x03C8, [ 0x2045 ], [ 0x20 ]);
      (0x03C9, [ 0x2047 ], [ 0x20 ]);
      (0x03D0, [ 0x2020 ], [ 0x20 ]);
      (0x03D1, [ 0x202B ], [ 0x20 ]);
      (0x03D2, [ 0x2042 ], [ 0x20 ]);
      (0x03D5, [ 0x2043 ], [ 0x20 ]);
      (0x03D6, [ 0x2035 ], [ 0x20 ]);
      (0x03D7, [ 0x202E; 0x201F; 0x202C ], [ 0x20; 0x20; 0x20 ]);
      (0x03D9, [ 0x2039 ], [ 0x20 ]);
      (0x03DB, [ 0x2027 ], [ 0x20 ]);
      (0x03DD, [ 0x2025 ], [ 0x20 ]);
      (0x03DF, [ 0x2038 ], [ 0x20 ]);
      (0x03E1, [ 0x2049 ], [ 0x20 ]);
      (0x03E3, [ 0x206B ], [ 0x20 ]);
      (0x03E5, [ 0x2070 ], [ 0x20 ]);
      (0x03E7, [ 0x2071 ], [ 0x20 ]);
      (0x03E9, [ 0x2074 ], [ 0x20 ]);
      (0x03EB, [ 0x207B ], [ 0x20 ]);
      (0x03ED, [ 0x207E ], [ 0x20 ]);
      (0x03EF, [ 0x2082 ], [ 0x20 ]);
      (0x03F0, [ 0x202E ], [ 0x20 ]);
      (0x03F1, [ 0x203A ], [ 0x20 ]);
      (0x03F2, [ 0x203D ], [ 0x20 ]);
      (0x03F3, [ 0x202D ], [ 0x20 ]);
      (0x03F5, [ 0x2024 ], [ 0x20 ]);
      (0x03F8, [ 0x204B ], [ 0x20 ]);
      (0x03FB, [ 0x2037 ], [ 0x20 ]);
      (0x03FC, [ 0x203C ], [ 0x20 ]);
      (0x0430, [ 0x2088 ], [ 0x20 ]);
      (0x0431, [ 0x2094 ], [ 0x20 ]);
      (0x0432, [ 0x2098 ], [ 0x20 ]);
      (0x0433, [ 0x209C ], [ 0x20 ]);
      (0x0434, [ 0x20B0 ], [ 0x20 ]);
      (0x0435, [ 0x20C0 ], [ 0x20 ]);
      (0x0436, [ 0x20C8 ], [ 0x20 ]);
      (0x0437, [ 0x20D2 ], [ 0x20 ]);
      (0x0438, [ 0x20E6 ], [ 0x20 ]);
      (0x043A, [ 0x20FC ], [ 0x20 ]);
      (0x043B, [ 0x2116 ], [ 0x20 ]);
      (0x043C, [ 0x2129 ], [ 0x20 ]);
      (0x043D, [ 0x2132 ], [ 0x20 ]);
      (0x043E, [ 0x214D ], [ 0x20 ]);
      (0x043F, [ 0x2155 ], [ 0x20 ]);
      (0x0440, [ 0x2162 ], [ 0x20 ]);
      (0x0441, [ 0x216B ], [ 0x20 ]);
      (0x0442, [ 0x2174 ], [ 0x20 ]);
      (0x0443, [ 0x2183 ], [ 0x20 ]);
      (0x0444, [ 0x2194 ], [ 0x20 ]);
      (0x0445, [ 0x2198 ], [ 0x20 ]);
      (0x0446, [ 0x21BF ], [ 0x20 ]);
      (0x0447, [ 0x21CA ], [ 0x20 ]);
      (0x0448, [ 0x21E9 ], [ 0x20 ]);
      (0x0449, [ 0x21EE ], [ 0x20 ]);
      (0x044A, [ 0x21F5 ], [ 0x20 ]);
      (0x044B, [ 0x21FA ], [ 0x20 ]);
      (0x044C, [ 0x21FE ], [ 0x20 ]);
      (0x044D, [ 0x220B ], [ 0x20 ]);
      (0x044E, [ 0x220F ], [ 0x20 ]);
      (0x044F, [ 0x2215 ], [ 0x20 ]);
      (0x0452, [ 0x20B6 ], [ 0x20 ]);
      (0x0454, [ 0x20C4 ], [ 0x20 ]);
      (0x0455, [ 0x20DA ], [ 0x20 ]);
      (0x0456, [ 0x20EE ], [ 0x20 ]);
      (0x0458, [ 0x20F7 ], [ 0x20 ]);
      (0x0459, [ 0x2122 ], [ 0x20 ]);
      (0x045A, [ 0x2148 ], [ 0x20 ]);
      (0x045B, [ 0x217F ], [ 0x20 ]);
      (0x045F, [ 0x21E5 ], [ 0x20 ]);
      (0x0461, [ 0x21AE ], [ 0x20 ]);
      (0x0463, [ 0x2206 ], [ 0x20 ]);
      (0x0465, [ 0x221A ], [ 0x20 ]);
      (0x0467, [ 0x221E ], [ 0x20 ]);
      (0x0469, [ 0x2228 ], [ 0x20 ]);
      (0x046B, [ 0x2223 ], [ 0x20 ]);
      (0x046D, [ 0x222D ], [ 0x20 ]);
      (0x046F, [ 0x2231 ], [ 0x20 ]);
      (0x0471, [ 0x2235 ], [ 0x20 ]);
      (0x0473, [ 0x2239 ], [ 0x20 ]);
      (0x0475, [ 0x223D ], [ 0x20 ]);
      (0x0479, [ 0x2190 ], [ 0x20 ]);
      (0x047B, [ 0x21BB ], [ 0x20 ]);
      (0x047D, [ 0x21B7 ], [ 0x20 ]);
      (0x047F, [ 0x21B2 ], [ 0x20 ]);
      (0x0481, [ 0x215E ], [ 0x20 ]);
      (0x0483, [], [ 0x50 ]);
      (0x0484, [], [ 0x33 ]);
      (0x0485, [], [ 0x23 ]);
      (0x0486, [], [ 0x22 ]);
      (0x0487, [], [ 0x33 ]);
      (0x048B, [ 0x20EA ], [ 0x20 ]);
      (0x048D, [ 0x2202 ], [ 0x20 ]);
      (0x048F, [ 0x2166 ], [ 0x20 ]);
      (0x0491, [ 0x209C ], [ 0x20; 0x112 ]);
      (0x0493, [ 0x20A0 ], [ 0x20 ]);
      (0x0495, [ 0x20A8 ], [ 0x20 ]);
      (0x0497, [ 0x20CE ], [ 0x20 ]);
      (0x0499, [ 0x20BC ], [ 0x20 ]);
      (0x049B, [ 0x2100 ], [ 0x20 ]);
      (0x049D, [ 0x2110 ], [ 0x20 ]);
      (0x049F, [ 0x210C ], [ 0x20 ]);
      (0x04A1, [ 0x2108 ], [ 0x20 ]);
      (0x04A3, [ 0x213B ], [ 0x20 ]);
      (0x04A5, [ 0x2144 ], [ 0x20 ]);
      (0x04A7, [ 0x215A ], [ 0x20 ]);
      (0x04A9, [ 0x2242 ], [ 0x20 ]);
      (0x04AB, [ 0x2170 ], [ 0x20 ]);
      (0x04AD, [ 0x217A ], [ 0x20 ]);
      (0x04AF, [ 0x2187 ], [ 0x20 ]);
      (0x04B1, [ 0x218B ], [ 0x20 ]);
      (0x04B3, [ 0x21A4 ], [ 0x20 ]);
      (0x04B5, [ 0x21C5 ], [ 0x20 ]);
      (0x04B7, [ 0x21D0 ], [ 0x20 ]);
      (0x04B9, [ 0x21D8 ], [ 0x20 ]);
      (0x04BB, [ 0x21A8 ], [ 0x20 ]);
      (0x04BD, [ 0x21DD ], [ 0x20 ]);
      (0x04BF, [ 0x21E1 ], [ 0x20 ]);
      (0x04C4, [ 0x2104 ], [ 0x20 ]);
      (0x04C6, [ 0x211B ], [ 0x20 ]);
      (0x04C8, [ 0x213F ], [ 0x20 ]);
      (0x04CA, [ 0x2137 ], [ 0x20 ]);
      (0x04CC, [ 0x21D4 ], [ 0x20 ]);
      (0x04CE, [ 0x212D ], [ 0x20 ]);
      (0x04CF, [ 0x2247 ], [ 0x20 ]);
      (0x04D5, [ 0x2090 ], [ 0x20 ]);
      (0x04D9, [ 0x208C ], [ 0x20 ]);
      (0x04E1, [ 0x20DF ], [ 0x20 ]);
      (0x04E9, [ 0x2151 ], [ 0x20 ]);
      (0x04F7, [ 0x20AC ], [ 0x20 ]);
      (0x04FB, [ 0x20A4 ], [ 0x20 ]);
      (0x04FD, [ 0x219C ], [ 0x20 ]);
      (0x04FF, [ 0x21A0 ], [ 0x20 ]);
      (0x0501, [ 0x20B4 ], [ 0x20 ]);
      (0x0503, [ 0x20BB ], [ 0x20 ]);
      (0x0505, [ 0x20D7 ], [ 0x20 ]);
      (0x0507, [ 0x20E4 ], [ 0x20 ]);
      (0x0509, [ 0x2127 ], [ 0x20 ]);
      (0x050B, [ 0x214C ], [ 0x20 ]);
      (0x050D, [ 0x216F ], [ 0x20 ]);
      (0x050F, [ 0x2179 ], [ 0x20 ]);
      (0x0511, [ 0x20D8 ], [ 0x20 ]);
      (0x0513, [ 0x2120 ], [ 0x20 ]);
      (0x0515, [ 0x2128 ], [ 0x20 ]);
      (0x0517, [ 0x216A ], [ 0x20 ]);
      (0x0519, [ 0x2219 ], [ 0x20 ]);
      (0x051B, [ 0x2115 ], [ 0x20 ]);
      (0x051D, [ 0x2246 ], [ 0x20 ]);
      (0x051F, [ 0x2114 ], [ 0x20 ]);
      (0x0521, [ 0x2121 ], [ 0x20 ]);
      (0x0523, [ 0x2143 ], [ 0x20 ]);
      (0x0525, [ 0x2159 ], [ 0x20 ]);
      (0x0527, [ 0x21AC ], [ 0x20 ]);
      (0x0529, [ 0x2136 ], [ 0x20 ]);
      (0x052B, [ 0x20CC ], [ 0x20 ]);
      (0x052D, [ 0x21CE ], [ 0x20 ]);
      (0x052F, [ 0x211F ], [ 0x20 ]);
      (0x1E9A, [ 0x1CAD; 0x1FE6 ], [ 0x20; 0x20 ]);
      (0x1E9C, [ 0x1EE6 ], [ 0x20 ]);
      (0x1E9D, [ 0x1EE7 ], [ 0x20 ]);
      (0x1E9F, [ 0x1D0F ], [ 0x20 ]);
      (0x1EFB, [ 0x1DDD; 0x1DDD ], [ 0x20; 0x20 ]);
      (0x1EFD, [ 0x1F56 ], [ 0x20 ]);
      (0x1EFF, [ 0x1F81 ], [ 0x20 ]);
    ];
  t

let ascii =
  Array.init 128 (fun c ->
      match
        Hashtbl.find_opt weights (Char.code (Char.lowercase_ascii (Char.chr c)))
      with
      | Some (p, _) -> p
      | None -> [ c lsl 4 ])

(* Standard tailorings of the Common Locale Data Repository (CLDR 50,
   common/collation/<language>.xml) for the languages of GeneWeb whose order
   differs from the common one *)
let rules =
  [
    ("af", {|&N<<<ŉ|});
    ("bg", {|[reorder Cyrl]|});
    ( "br",
      {|&C<ch<<<Ch<<<CH<c''h=c\u02BCh=c\u2019h<<<C''h=C\u02BCh=C\u2019h<<<C''H=C\u02BCH=C\u2019H
       &ker<<ꝃ<<<Ꝃ|}
    );
    ( "cs",
      {|&C<č<<<Č
       &H<ch<<<cH<<<Ch<<<CH
       &R<ř<<<Ř
       &S<š<<<Š
       &Z<ž<<<Ž|}
    );
    ( "da",
      {|[caseFirst upper]
       &D<<đ<<<Đ<<ð<<<Ð
       &th<<<þ
       &TH<<<Þ
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<å<<<Å<<<aa<<<Aa<<<AA
       &oe<<œ<<<Œ|}
    );
    ( "eo",
      {|&C<ĉ<<<Ĉ
       &G<ĝ<<<Ĝ
       &H<ĥ<<<Ĥ
       &J<ĵ<<<Ĵ
       &S<ŝ<<<Ŝ
       &U<ŭ<<<Ŭ|}
    );
    ("es", {|&N<ñ<<<Ñ|});
    ( "et",
      {|&[before 1]T<š<<<Š<z<<<Z<ž<<<Ž
       &[before 1]X<õ<<<Õ<ä<<<Ä<ö<<<Ö<ü<<<Ü|}
    );
    ( "fi",
      {|&D\u0335<<đ<<<Đ
       &G\u0335<<ǥ<<<Ǥ
       &N\u0335<<ŋ<<<Ŋ
       &T\u0335<<ŧ<<<Ŧ
       &Y<<ü<<<Ü
       &Z\u0335<<ʒ<<<Ʒ
       &[before 1]ǀ<å<<<Å<ä<<<Ä<<æ<<<Æ<ö<<<Ö<<ø<<<Ø|}
    );
    ( "is",
      {|&[before 1]b<á<<<Á
       &d<<đ<<<Đ<ð<<<Ð
       &[before 1]f<é<<<É
       &[before 1]j<í<<<Í
       &[before 1]p<ó<<<Ó
       &[before 1]v<ú<<<Ú
       &[before 1]z<ý<<<Ý
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<ö<<<Ö<<ø<<<Ø<å<<<Å|}
    );
    ( "lt",
      {|&̀=̇̀
       &́=̇́
       &̃=̇̃
       &A<<ą<<<Ą
       &C<č<<<Č
       &E<<ę<<<Ę<<ė<<<Ė
       &I<<į<<<Į<<y<<<Y
       &S<š<<<Š
       &U<<ų<<<Ų<<ū<<<Ū
       &Z<ž<<<Ž|}
    );
    ( "lv",
      {|&A<ā<<<Ā
       &C<č<<<Č
       &E<ē<<<Ē
       &G<ģ<<<Ģ
       &I<y<<<Y<ī<<<Ī
       &K<ķ<<<Ķ
       &L<ļ<<<Ļ
       &N<ņ<<<Ņ
       &O<ō<<<Ō
       &R<ŗ<<<Ŗ
       &S<š<<<Š
       &U<ū<<<Ū
       &Z<ž<<<Ž|}
    );
    ( "no",
      {|&D<<đ<<<Đ<<ð<<<Ð
       &t<<<þ/h
       &T<<<Þ/H
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<<ę<<<Ę<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<<œ<<<Œ<å<<<Å<<aa<<<Aa<<<AA|}
    );
    ( "pl",
      {|&A<ą<<<Ą
       &C<ć<<<Ć
       &E<ę<<<Ę
       &L<ł<<<Ł
       &N<ń<<<Ń
       &O<ó<<<Ó
       &S<ś<<<Ś
       &Z<ź<<<Ź<ż<<<Ż|}
    );
    ( "ro",
      {|&A<ă<<<Ă<â<<<Â
       &I<î<<<Î
       &S<ş=ș<<<Ş=Ș
       &T<ţ=ț<<<Ţ=Ț|}
    );
    ("ru", {|[reorder Cyrl]|});
    ( "sk",
      {|&A<ä<<<Ä
       &C<č<<<Č
       &H<ch<<<cH<<<Ch<<<CH
       &O<ô<<<Ô
       &R<ř<<<Ř
       &S<š<<<Š
       &Z<ž<<<Ž|}
    );
    ("sl", {|&C<č<<<Č<ć<<<Ć
       &D<đ<<<Đ
       &S<š<<<Š
       &Z<ž<<<Ž|});
    ( "sv",
      {|&D<<đ<<<Đ<<ð<<<Ð
       &t<<<þ/h
       &T<<<Þ/H
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<å<<<Å<ä<<<Ä<<æ<<<Æ<<ę<<<Ę<ö<<<Ö<<ø<<<Ø<<ő<<<Ő<<œ<<<Œ<<ô<<<Ô|}
    );
    ( "tr",
      {|&C<ç<<<Ç
       &G<ğ<<<Ğ
       &[before 1]i<ı<<<I
       &i<<<İ
       &O<ö<<<Ö
       &S<ş<<<Ş
       &U<ü<<<Ü|}
    );
  ]

type tailoring = {
  items : (string, int list * int list) Hashtbl.t;
  mutable contractions : string list;
  mutable upper_first : bool;
  mutable cyrillic_first : bool;
}

let root =
  {
    items = Hashtbl.create 1;
    contractions = [];
    upper_first = false;
    cyrillic_first = false;
  }

let current = ref root

let decompose s =
  let n = Uunf.create `NFD in
  let us = ref [] in
  let rec add v =
    match Uunf.add n v with
    | `Uchar u ->
        us := u :: !us;
        add `Await
    | `Await | `End -> ()
  in
  Uutf.String.fold_utf_8
    (fun () _ -> function `Uchar _ as u -> add u | `Malformed _ -> ())
    () s;
  add `End;
  List.rev !us

let lower u =
  match Uucp.Case.Map.to_lower u with `Uchars (u :: _) -> u | _ -> u

let next s i =
  let n = try Utf8.nbc s.[i] with Invalid_argument _ -> 1 in
  min (String.length s) (i + n)

(* the primary and secondary weights of the character at [i] in [s], and the
   position of the next one; the letters of other scripts after these ones,
   by code point, the other characters by their transliteration *)
let root_char t s i =
  let c = Char.code (String.unsafe_get s i) in
  if c < 0x80 then (ascii.(c), [ 0x20 ], i + 1)
  else
    let j = next s i in
    let ch = String.sub s i (j - i) in
    let p, w =
      List.split
      @@ List.map
           (fun u ->
             let l = Uchar.to_int (lower u) in
             match Hashtbl.find_opt weights l with
             | Some (p, w) when t.cyrillic_first && l >= 0x400 && l < 0x530 ->
                 (List.map (fun w -> w - 0x8000) p, w)
             | Some w -> w
             | None -> (
                 match Uucp.Gc.general_category u with
                 | `Lu | `Ll | `Lt | `Lm | `Lo ->
                     ([ (0x10000 + l) lsl 4 ], [ 0x20 ])
                 | `Mn | `Mc | `Me -> ([], [])
                 | _ ->
                     let tr, _ = Name.unaccent_utf_8 false ch 0 in
                     let p =
                       List.concat_map
                         (fun c -> ascii.(Char.code c land 0x7F))
                         (List.of_seq (String.to_seq tr))
                     in
                     (p, List.map (fun _ -> 0x20) p)))
           (decompose ch)
    in
    (List.concat p, List.concat w, j)

let matches s i k =
  let n = String.length k in
  i + n <= String.length s
  &&
  let rec loop j = j >= n || (s.[i + j] = k.[j] && loop (j + 1)) in
  loop 0

(* as [root_char], the contractions and letters of the tailoring [t] first *)
let char t s i =
  if t == root then root_char t s i
  else
    match List.find_opt (matches s i) t.contractions with
    | Some k ->
        let p, w = Hashtbl.find t.items k in
        (p, w, i + String.length k)
    | None -> (
        let j = next s i in
        let ch = String.sub s i (j - i) in
        match Hashtbl.find_opt t.items ch with
        | Some (p, w) -> (p, w, j)
        | None -> (
            match
              Hashtbl.find_opt t.items
                (if j - i = 1 then String.lowercase_ascii ch
                 else Utf8.cmap_utf_8 Uucp.Case.Map.to_lower ch)
            with
            | Some (p, w) -> (p, w, j)
            | None -> root_char t s i))

let weights_of t s =
  let rec loop i p w =
    if i >= String.length s then
      (List.concat (List.rev p), List.concat (List.rev w))
    else
      let p', w', i = char t s i in
      loop i (p' :: p) (w' :: w)
  in
  loop 0 [] []

let unescape r =
  let n = String.length r in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then
      match r.[i] with
      | '\\' when i + 5 < n && r.[i + 1] = 'u' ->
          Uutf.Buffer.add_utf_8 b
            (Uchar.of_int (int_of_string ("0x" ^ String.sub r (i + 2) 4)));
          loop (i + 6)
      | '\'' when i + 1 < n && r.[i + 1] = '\'' ->
          Buffer.add_char b '\'';
          loop (i + 2)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

(* The tailoring of the rules [r]: resets [&x], [&[before 1]x], relations
   [<] (primary), [<<] (secondary), [<<<] and [=] (same weights, the case
   being compared apart), expansions [y/x], options [[caseFirst upper]] and
   [[reorder Cyrl]] *)
let tailor r =
  let t =
    {
      items = Hashtbl.create 64;
      contractions = [];
      upper_first = false;
      cyrillic_first = false;
    }
  in
  let r = Mutil.normalize_utf_8 (unescape r) in
  let n = String.length r in
  let blank c = c = ' ' || c = '\n' || c = '\t' || c = '\r' in
  let rec skip i = if i < n && blank r.[i] then skip (i + 1) else i in
  let word i =
    let i = skip i in
    let j = ref i in
    while !j < n && not (blank r.[!j] || String.contains "&<=/[" r.[!j]) do
      incr j
    done;
    (String.sub r i (!j - i), !j)
  in
  let bump p d =
    match List.rev p with w :: l -> List.rev ((w + d) :: l) | [] -> [ d ]
  in
  let rec loop i base cur sec =
    let i = skip i in
    if i < n then
      match r.[i] with
      | '[' ->
          let j = String.index_from r i ']' in
          (match String.sub r i (j - i + 1) with
          | "[caseFirst upper]" -> t.upper_first <- true
          | "[reorder Cyrl]" -> t.cyrillic_first <- true
          | _ -> ());
          loop (j + 1) base cur sec
      | '&' ->
          let i = skip (i + 1) in
          let before = i < n && r.[i] = '[' in
          let i = if before then String.index_from r i ']' + 1 else i in
          let x, i = word i in
          let p, w = weights_of t x in
          let x = ((if before then bump p (-16) else p), w) in
          loop i x x 0
      | c ->
          let k =
            if c = '=' then 1
            else if i + 2 < n && r.[i + 1] = '<' && r.[i + 2] = '<' then 3
            else if i + 1 < n && r.[i + 1] = '<' then 2
            else 1
          in
          let y, i = word (i + k) in
          let x, i = if i < n && r.[i] = '/' then word (i + 1) else ("", i) in
          let ((p, w) as v), base, sec =
            match (c, k) with
            | '<', 1 ->
                let p = bump (fst cur) 1 in
                let v = (p, List.map (fun _ -> 0x20) p) in
                (v, v, 0)
            | '<', 2 ->
                ((fst base, snd base @ [ 0x200 + sec + 1 ]), base, sec + 1)
            | _ -> (cur, base, sec)
          in
          let xp, xw = weights_of t x in
          Hashtbl.replace t.items y (p @ xp, w @ xw);
          if Utf8.length y > 1 then t.contractions <- y :: t.contractions;
          loop i base v sec
  in
  loop 0 ([], []) ([], []) 0;
  t.contractions <-
    List.sort
      (fun a b -> Int.compare (String.length b) (String.length a))
      t.contractions;
  t

(* The sort key of [s] (Unicode collation algorithm, 7.3): its primary,
   secondary and tertiary weights, the tertiary being the case of each
   character, then [s] itself, so that only equal strings have equal keys *)
let key t s =
  let rec loop i p w c =
    if i < String.length s then
      let p', w', j = char t s i in
      let upper =
        match s.[i] with
        | 'A' .. 'Z' -> true
        | '\000' .. '\127' -> false
        | _ -> (
            try
              let u = Utf8.C.cp s i in
              Uucp.Case.is_upper u || Uucp.Gc.general_category u = `Lt
            with Invalid_argument _ -> false)
      in
      let case = if not upper then 2 else if t.upper_first then 1 else 8 in
      loop j (p' :: p) (w' :: w) (case :: c)
    else (List.concat (List.rev p), List.concat (List.rev w), List.rev c)
  in
  let p, w, c = loop 0 [] [] [] in
  let b = Buffer.create (8 * String.length s) in
  List.iter (fun x -> Buffer.add_int32_be b (Int32.of_int x)) p;
  Buffer.add_int32_be b 0l;
  List.iter (Buffer.add_uint16_be b) w;
  Buffer.add_uint16_be b 0;
  List.iter (Buffer.add_uint8 b) c;
  Buffer.add_uint8 b 0;
  Buffer.add_string b s;
  Buffer.contents b

(* the sort keys already computed, for the tailoring [!current] *)
let keys = Hashtbl.create 1024

let compare a b =
  let key_of s =
    match Hashtbl.find_opt keys s with
    | Some k -> k
    | None ->
        if Hashtbl.length keys >= 100_000 then Hashtbl.reset keys;
        let k = key !current s in
        Hashtbl.add keys s k;
        k
  in
  if a = b then 0 else String.compare (key_of a) (key_of b)

let tailorings = Hashtbl.create 4

let set_lang lang =
  let lang =
    match String.index_opt lang '-' with
    | Some i -> String.sub lang 0 i
    | None -> lang
  in
  let t =
    match List.assoc_opt lang rules with
    | None -> root
    | Some r -> (
        match Hashtbl.find_opt tailorings lang with
        | Some t -> t
        | None ->
            let t = tailor r in
            Hashtbl.replace tailorings lang t;
            t)
  in
  if t != !current then (
    current := t;
    Hashtbl.reset keys)

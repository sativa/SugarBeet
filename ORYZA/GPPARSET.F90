subroutine GPParSet (xCO2, xKNF, xNFLV, xREDFT)
use gp
implicit none
real xCO2, xKNF, xNFLV, xREDFT

!real cCO2, cKNF, cNFLV, cREDFT
!common /gp_common/ cCO2, cKNF, cNFLV, cREDFT

! store variables in common block
cCO2   = xCO2
cKNF   = xKNF
cNFLV  = xNFLV
cREDFT = xREDFT

return
end

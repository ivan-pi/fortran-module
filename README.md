# Fortran module for preCICE

This is a Fortran module that uses the [preCICE Fortran bindings](https://precice.org/couple-your-code-api.html) (written in C++) using the `iso_c_binding` intrinsic module.

Build this module using `make`. This executes:

```shell
gfortran -c precice.f90
```

This project was moved from the [main preCICE repository](https://github.com/precice/precice). See the [history](https://github.com/precice/precice/tree/d0fafbd912ad6cbf0727299d23e1210570957945/src/precice/bindings/f2003). Previous contributions by @haraldkl, @Krupp, @gatzhamm, @uekerman, @floli, @MakisH, @BenjaminRueth, @RPGP1.

---

### Precice Participant API

- Object creation
    * [x] Participant (structure constructor)
    * [ ] destroy_participant (finalizer)

- Steering Methods
    * [x] initialize
    * [x] advance
    * [x] finalize
- Implicit Coupling
    * [x] requiresWritingCheckpoint
    * [x] requiresReadingCheckpoint
- Status Queries
    * [x] getMeshDimensions
    * [x] getDataDimensions
    * [x] isCouplingOngoing
    * [x] isTimeWindowComplete
    * [x] getMaxTimeStepSize
- MeshAccess
    * [ ] requiresMeshConnectivityFor
    * [ ] getMeshVertexSize
    * [ ] setMeshVertex
    * [ ] setMeshVertices
    * [ ] setMeshEdge
    * [ ] setMeshEdges
    * [ ] setMeshTriangle
    * [ ] setMeshTriangles
    * [ ] setMeshQuad
    * [ ] setMeshQuads
    * [ ] setMeshTetraHedron
    * [ ] setMeshTetraHedra
- Data Access
    * [ ] requiresInitialData
    * [ ] writeData
    * [ ] readData
- Direct Access
    * [x] setMeshAccessRegion
    * [x] getMeshVertexIDsAndCoordinates
- Gradient Data (Experimental)
    * [x] requiresGradientDataFor
    * [x] writeGradientData
- Other
    * [ ] getVersionInformation

### Precice Tooling API

Methods:
- printConfigReference
- checkConfiguration

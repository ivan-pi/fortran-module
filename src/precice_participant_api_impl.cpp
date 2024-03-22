
#include <string>
#include <ISO_Fortran_binding.h>
#include "precice/precice.hpp"


constexpr auto view(CFI_cdesc_t *fstr) {
    return std::string_view{ 
        (char *) fstr->base_addr, 
        (size_t) fstr->elem_len };
}

using namespace precice;
typedef void * PHandle;

extern "C" {


PHandle pfapi_default_participant(
        CFI_cdesc_t *participantName,
        CFI_cdesc_t *configurationFileName,
        int solverProcessIndex,
        int solverProcessSize)
{
    return static_cast<PHandle>(new Participant(view(participantName),view(configurationFileName),
        solverProcessSize,solverProcessIndex));
}

#if defined(WITH_MPI)
PHandle pfapi_mpi_participant(
        CFI_cdesc_t *participantName,
        CFI_cdesc_t *configurationFileName,
        int solverProcessIndex,
        int solverProcessSize,
        void *comm)
{
    return static_cast<PHandle>(new Participant(view(participantName),view(configurationFileName),
        solverProcessSize,solverProcessIndex,comm));
}
#endif

void pfapi_initialize(PHandle ph) 
{
    auto participant = static_cast<Participant *>(ph);
    participant->initialize();
}

void pfapi_advance(PHandle ph, double computedTimeStepSize) 
{
    auto participant = static_cast<Participant *>(ph);
    participant->advance(computedTimeStepSize);
}

void pfapi_finalize(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);
    participant->finalize();
}

int pfapi_getmeshdimensions(PHandle ph, CFI_cdesc_t *meshName)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->getMeshDimensions(view(meshName));
}

int pfapi_getdatadimensions(PHandle ph, CFI_cdesc_t *meshName, CFI_cdesc_t *dataName)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->getDataDimensions(view(meshName),view(dataName));
}

bool pfapi_iscouplingongoing(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->isCouplingOngoing();
}

bool pfapi_istimewindowcomplete(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->isTimeWindowComplete();
}

bool pfapi_getmaxtimestepsize(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->getMaxTimeStepSize();  
}

bool pfapi_requirestnitialdata(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);  
    return participant->requiresInitialData();
}

bool pfapi_requireswritingcheckpoint(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);  
    return participant->requiresWritingCheckpoint();
}

bool pfapi_requiresreadingcheckpoint(PHandle ph)
{
    auto participant = static_cast<Participant *>(ph);  
    return participant->requiresReadingCheckpoint();
}

bool pfapi_requiresmeshconnectivityfor(PHandle ph, CFI_cdesc_t *meshName)
{
    auto participant = static_cast<Participant *>(ph);  
    return participant->requiresMeshConnectivityFor(view(meshName));
}

int pfapi_setmeshvertex(PHandle ph, CFI_cdesc_t *meshName, CFI_cdesc_t *position)
{
    auto participant = static_cast<Participant *>(ph);

    auto name = view(meshName);

    auto dim = static_cast<size_t>(participant->getMeshDimensions(name));
    return participant->setMeshVertex(
        name, 
        {(double *) position->base_addr, dim});
}


void pfapi_setmeshvertices(PHandle ph,
    const char *  meshName,
    int           size,
    const double *positions,
    int *         ids)
{
    auto participant = static_cast<Participant *>(ph);
    auto idsSize = static_cast<long unsigned>(size);
    auto posSize = static_cast<long unsigned>(participant->getMeshDimensions(meshName) * size);
    participant->setMeshVertices(meshName, {positions, posSize}, {ids, idsSize});
}

int pfapi_getmeshvertexsize(PHandle ph, CFI_cdesc_t *meshName)
{
    auto participant = static_cast<Participant *>(ph);
    return participant->getMeshVertexSize(view(meshName));
}

void pfapi_writedata(PHandle ph,
    CFI_cdesc_t *  meshName,
    CFI_cdesc_t *  dataName,
    int           size,
    const int *   valueIndices,
    const double *values)
{
    auto mname = view(meshName);
    auto dname = view(dataName);
    auto participant = static_cast<Participant *>(ph);
    auto dataSize = size * participant->getDataDimensions(mname, dname);
    participant->writeData(mname, dname, {valueIndices, static_cast<unsigned long>(size)}, {values, static_cast<unsigned long>(dataSize)});
}

void pfapi_readdata(PHandle ph,
    CFI_cdesc_t *meshName,
    CFI_cdesc_t *dataName,
    int         size,
    const int * valueIndices,
    double      relativeReadTime,
    double *    values)
{
    auto mname = view(meshName);
    auto dname = view(dataName);
    auto p = static_cast<Participant *>(ph);
    auto dataSize = size * p->getDataDimensions(mname, dname);
    p->readData(mname,dname, {valueIndices, static_cast<unsigned long>(size)}, relativeReadTime, {values, static_cast<unsigned long>(dataSize)});
}

bool pfapi_requiresgradientdatafor(PHandle ph, CFI_cdesc_t *meshName, CFI_cdesc_t *dataName)
{
    auto p = static_cast<Participant *>(ph);
    return p->requiresGradientDataFor(view(meshName), view(dataName));
}

void pfapi_writegradientdata(PHandle ph,
    CFI_cdesc_t *  meshName,
    CFI_cdesc_t *  dataName,
    int           size,
    const int *   valueIndices,
    const double *gradients)
{
    auto mname = view(meshName);
    auto dname = view(dataName);
    auto p = static_cast<Participant *>(ph);
    auto gradientComponents = p->getDataDimensions(mname, dname) * p->getMeshDimensions(mname);
    auto gradientSize       = size * gradientComponents;
    p->writeGradientData(mname, dname, {valueIndices, static_cast<unsigned long>(size)}, {gradients, static_cast<unsigned long>(gradientSize)});
}



} // extern "C"
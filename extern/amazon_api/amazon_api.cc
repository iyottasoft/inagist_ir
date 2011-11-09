#include "amazon_api.h"
#include <iostream>

/*
the function calls to make cpp talk to python are
written with help from the following tutorial
http://www.codeproject.com/KB/cpp/embedpython_1.aspx
*/


namespace inagist_api {

AmazonAPI::AmazonAPI(const char* program_name) {

  // initialize the python intrepreter
  char pySearchPath[] = "/usr/lib/python2.6/config/";
  Py_SetPythonHome(pySearchPath);

  Py_SetProgramName((char *) program_name);

  Py_Initialize();
/*
  PyRun_SimpleString("import amazon_api");
  PyRun_SimpleString("a = amazon_api.BottleNoseAPI()");
  PyRun_SimpleString("a.item_search()");
*/

}

AmazonAPI::~AmazonAPI() {
  Clear();
}

int AmazonAPI::Init(const char* module_name, const char* class_name) {

  if (!module_name || !class_name) {
    std::cerr << "ERROR: invalid inputs given to initialize AmazonAPI\n";
    return -1;
  }

  // build the name object
  m_pyModuleName = PyString_FromString(module_name);
  PyErr_Print();
  if (!m_pyModuleName) {
    std::cerr << "ERROR: invalid python value for module " << module_name << std::endl;
    PyErr_Print();
    return -1;
  }

/*
  PyRun_SimpleString("import sys");
  PyErr_Print();
  PyRun_SimpleString("sys.path.append(\"~/ir_cpp/extern/amazon_api/\")");
  PyErr_Print();
  PyRun_SimpleString("import amazon_api");
  PyErr_Print();
*/

  // load the module object
  m_pyModule = PyImport_Import(m_pyModuleName);
  if (!m_pyModule) {
    std::cerr << "ERROR: could not import python module " << module_name << std::endl;
    PyErr_Print();
    return -1;
  } else {
    std::cout << "obtained pyModule " << module_name << ". done." << std::endl;
  }

  // pyDict is a borrowed reference
  m_pyDict = PyModule_GetDict(m_pyModule);
  if (!m_pyDict) {
    std::cerr << "ERROR: could not obtain a reference to dict\n";
    PyErr_Print();
    return -1;
  }

  // build the name of the callable class
  m_pyClass = PyDict_GetItemString(m_pyDict, class_name);
  if (!m_pyClass) {
    std::cerr << "ERROR: couldn't build python name for class " << class_name << std::endl;
    PyErr_Print();
    return -1;
  }

  // create an instance of a class
  if (PyCallable_Check(m_pyClass)) {
    m_pyInstance = PyObject_CallObject(m_pyClass, NULL);
    if (!m_pyInstance) {
      std::cerr << "ERROR: could not create instance of class " << class_name << std::endl;
      PyErr_Print();
      return -1;
    }
  }

  return 0;
}

int AmazonAPI::ItemSearch() {
  char item_search[] = "item_search";

  m_pyValue = PyObject_CallMethod(m_pyInstance, (char *) item_search, NULL);
  if (m_pyValue != NULL) {
    std::cout << "a valid python value returned\n";
  } else {
    PyErr_Print();
    return -1;
  }

  return 0;
}

int AmazonAPI::Clear() {
  // Clean up

  if (m_pyModule) {
    Py_DECREF(m_pyModule);
  }
  if (m_pyModuleName) {
    Py_DECREF(m_pyModuleName);
  }

  // Finish the Python Interpreter

  Py_Finalize();

  return 0;
}

} // namespace inagist_api

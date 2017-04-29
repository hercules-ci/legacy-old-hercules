#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#include <nix/local-store.hh>
#include <nix/remote-store.hh>
#include <nix/store-api.hh>

namespace nix {

class HerculesStore : public Store {
public:
  ~HerculesStore();

  HerculesStore(const Store::Params &params, const std::string &something)
      : Store(params), BaseStore(params){};

  void buildPaths(const PathSet &paths, BuildMode buildMode) override;
  BuildResult buildDerivation(const Path & drvPath, const BasicDerivation & drv,
      BuildMode buildMode = bmNormal) override;
  void ensurePath(const Path &path) override;
};

template <class BaseStore> HerculesStore<BaseStore>::~HerculesStore() {}

template <class BaseStore>
void HerculesStore<BaseStore>::buildPaths(const PathSet &paths,
                                       BuildMode buildMode) {
  std::cerr << "buildPaths called for" << std::endl;
  std::for_each(paths.begin(), paths.end(), [](const Path &path) {
    std::cerr << "  - " << path << std::endl;
  });

  BaseStore::buildPaths(paths, buildMode);
}

template <class BaseStore>
BuildResult HerculesStore<BaseStore>::buildDerivation(const Path & drvPath, const BasicDerivation & drv, BuildMode buildMode) {
  std::cerr << "buildDerivation called for" << std::endl;
  std::cerr << "  - " << drvPath << std::endl;

  return BaseStore::buildDerivation(drvPath, drv, buildMode);
};

template <class BaseStore>
void HerculesStore<BaseStore>::ensurePath(const Path &path) {
  std::cerr << "ensurePath called for" << std::endl;
  std::cerr << "  - " << path << std::endl;

  BaseStore::ensurePath(path);
}

////////////////////////////////////////////////////////////////////////////////
// Register the store
////////////////////////////////////////////////////////////////////////////////

static RegisterStoreImplementation
regStore([](const std::string &uri,
            const Store::Params &params) -> std::shared_ptr<Store> {
  const std::string prefix = "dummy://";
  if (std::string(uri, 0, prefix.length()) != prefix) {
    return nullptr;
  }

  const std::string resource(uri, prefix.length());

  switch (getStoreType(resource, get(params, "state", settings.nixStateDir))) {
  case tDaemon:
    std::cerr
        << "Registering dummy store with UDSRemoteStore (daemon) underneath"
        << std::endl;
    return std::shared_ptr<Store>(
        std::make_shared<HerculesStore<UDSRemoteStore>>(params, ""));
  case tLocal:
    std::cerr << "Registering dummy store with LocalStore underneath"
              << std::endl;
    return std::shared_ptr<Store>(
        std::make_shared<HerculesStore<LocalStore>>(params, ""));
  default:
    return nullptr;
  }
});
};

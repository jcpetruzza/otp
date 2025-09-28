def _transition_ignore_constraints_impl(ctx: AnalysisContext) -> list[Provider]:
    name = ctx.attrs.name
    constraint_labels = [c[ConstraintSettingInfo].label for c in ctx.attrs.constraints]

    def _transition_impl(platform: PlatformInfo) -> PlatformInfo:
        constraints = platform.configuration.constraints
        values = platform.configuration.values

        to_ignore = set(constraint_labels)

        filtered_constraints = {
            s: v
            for s, v in constraints.items()
            if s not in to_ignore
        }

        return PlatformInfo(
            label = "<{}>".format(name),
            configuration = ConfigurationInfo(
                constraints = filtered_constraints,
                values = values
            ),
        )

    return [
        DefaultInfo(),
        TransitionInfo(impl = _transition_impl),
    ]

transition_ignore_constraints = rule(
    impl = _transition_ignore_constraints_impl,
    attrs = {
        "constraints": attrs.list(attrs.dep(providers=[ConstraintSettingInfo])),
    },
    is_configuration_rule = True,
)

def _transition_force_constraints_impl(ctx: AnalysisContext) -> list[Provider]:
    name = ctx.attrs.name
    forced_constraints = {
        c[ConstraintValueInfo].setting.label: c[ConstraintValueInfo]
        for c in ctx.attrs.constraints
    }

    def _transition_impl(platform: PlatformInfo) -> PlatformInfo:
        constraints = platform.configuration.constraints
        values = platform.configuration.values

        updated_constraints = dict(constraints)
        updated_constraints.update(forced_constraints)

        return PlatformInfo(
            label = "<{}>".format(name),
            configuration = ConfigurationInfo(
                constraints = updated_constraints,
                values = values
            ),
        )

    return [
        DefaultInfo(),
        TransitionInfo(impl=_transition_impl),
    ]

transition_force_constraints = rule(
    impl = _transition_force_constraints_impl,
    attrs = {
        "constraints": attrs.list(attrs.dep(providers=[ConstraintValueInfo])),
    },
    is_configuration_rule = True,
)
